package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try, Success, Failure}

import io.syspulse.skel.plugin.{Plugin, PluginDescriptor}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.core.Event

import io.syspulse.ext.sentinel.feeds._
import io.syspulse.skel.util.Util
import io.syspulse.skel.script.{Script, ScriptFlow}
import io.hacken.ext.sentinel.ScriptEngine
import io.hacken.ext.sentinel.ThresholdDouble

object DetectorNews {
  val DEF_CRON = "10 minutes"
  val DEF_FEEDS = ""
  val DEF_TYPE = ""  // Empty = parse URI prefixes (rss://, reddit://), "rss" = all RSS, "reddit" = all Reddit
  val DEF_DESC = "New post: {title}{err}"
  val DEF_MAX = 0  // 0 = no limit on posts to parse
  val DEF_MAX_SEEN_POSTS = 100
  val DEF_THRESHOLD = ">= 0.5"  // Default threshold for score-based filtering

  val DEF_TRACK_ERR = true
  val DEF_TRACK_ERR_ALWAYS = true

  val DEF_SEV_NEW_POST = Severity.INFO
  val DEF_SEV_ERR = Severity.ERROR

  val DEF_EID_HASH = true

  /**
   * Parse feed URI based on type configuration
   * @param uri The feed URI (may have rss:// or reddit:// prefix)
   * @param feedType The configured feed type ("", "rss", or "reddit")
   * @return Tuple of (actualFeedType: "rss" or "reddit", cleanedUri: String)
   */
  def parseFeedUri(uri: String, feedType: String): (String, String) = {
    feedType.toLowerCase match {
      case "rss" =>
        // All feeds are RSS
        ("rss", uri)

      case "reddit" =>
        // All feeds are Reddit
        ("reddit", uri)

      case "" =>
        // Parse URI prefix: rss://, reddit://, or assume RSS
        if (uri.startsWith("rss://")) {
          ("rss", uri.substring(6)) // Strip "rss://"
        } else if (uri.startsWith("reddit://")) {
          ("reddit", uri.substring(9)) // Strip "reddit://"
        } else {
          // No prefix, assume RSS
          ("rss", uri)
        }

      case _ =>
        // Unknown type, assume RSS
        ("rss", uri)
    }
  }
}

class DetectorNews(pd: PluginDescriptor) extends Sentry with Plugin {
  override def did = pd.name
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  def eid(post: NewsPost) = {
    if (DetectorNews.DEF_EID_HASH) {
      Some(Util.sha256(post.id))
    } else {
      Some(post.id)
    }
  }

  override def getSettings(rx: SentryRun): Map[String, Any] = {
    rx.getConfig().env match {
      case "test" => Map()
      case "dev" =>
        Map("_cron_rate_limit" -> 1 * 60 * 1000L) // 1 min for dev
      case _ =>
        Map("_cron_rate_limit" -> 10 * 60 * 1000L) // 10 min for prod
    }
  }

  override def onInit(rx: SentryRun, conf: DetectorConfig): Int = {
    val r = super.onInit(rx, conf)
    if (r != SentryRun.SENTRY_INIT) {
      return r
    }

    // Initialize seen posts set
    rx.set("seen_posts", Set.empty[String])

    SentryRun.SENTRY_INIT
  }

  override def onStart(rx: SentryRun, conf: DetectorConfig): Int = {
    val r = onUpdate(rx, conf)
    if (r != SentryRun.SENTRY_RUNNING) {
      return r
    }

    super.onStart(rx, conf)
  }

  override def onUpdate(rx: SentryRun, conf: DetectorConfig): Int = {
    // Parse feeds configuration
    val feedsStr = DetectorConfig.getString(conf, "feeds", DetectorNews.DEF_FEEDS)
    if (feedsStr.isEmpty) {
      log.warn(s"${rx.getExtId()}: feeds configuration required")
      error("Feeds configuration required", None)
      return SentryRun.SENTRY_STOPPED
    }

    // Get feed type configuration
    val feedType = DetectorConfig.getString(conf, "type", DetectorNews.DEF_TYPE).toLowerCase

    // Create feed instances based on type configuration
    val feeds: Seq[NewsFeed] = feedsStr.split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { uri =>
        val (actualFeedType, cleanedUri) = DetectorNews.parseFeedUri(uri, feedType)
        actualFeedType match {
          case "rss" => new RssFeed(cleanedUri)
          case "reddit" => new RedditFeed(cleanedUri)
          case _ => new RssFeed(cleanedUri) // Fallback
        }
      }

    log.info(s"${rx.getExtId()}: Configured feeds: ${feeds.size} (${feeds})")
    rx.set("feeds", feeds)

    // Configuration
    rx.set("desc", DetectorConfig.getString(conf, "desc", DetectorNews.DEF_DESC))

    // Load scripts and error tracking configuration using ScriptEngine
    ScriptEngine.loadConfig(rx, conf, DetectorNews.DEF_TRACK_ERR, DetectorNews.DEF_TRACK_ERR_ALWAYS)

    // Set threshold for score-based filtering
    val thresholdCondition = DetectorConfig.getString(conf, "threshold", DetectorNews.DEF_THRESHOLD)
    val threshold = new ThresholdDouble(0.0, thresholdCondition)
    rx.set("threshold", threshold)

    rx.set("max",DetectorConfig.getInt(conf, "max", DetectorNews.DEF_MAX))
    rx.set("max_seen_posts", DetectorConfig.getInt(conf, "max_seen_posts", DetectorNews.DEF_MAX_SEEN_POSTS))

    // Initialize seen posts with current feed state (don't alert on first run)
    //initializeSeenPosts(rx)

    SentryRun.SENTRY_RUNNING
  }

  private def initializeSeenPosts(rx: SentryRun): Unit = {
    val feeds = rx.get("feeds").get.asInstanceOf[Seq[NewsFeed]]

    val currentPosts = feeds.flatMap { feed =>
      feed.fetchFeed() match {
        case Success(posts) => posts
        case Failure(e) =>
          log.warn(s"${rx.getExtId()}: Failed to initialize: ${feed.getSource()}: ${e.getMessage}")
          Seq.empty
      }
    }

    val postIds = currentPosts.map(_.id).toSet
    rx.set("seen_posts", postIds)
    log.info(s"${rx.getExtId()}: Initialized: ${postIds.size} (seen posts)")
  }

  override def onCron(rx: SentryRun, elapsed: Long): Seq[Event] = {
    checkFeeds(rx)
  }

  def checkFeeds(rx: SentryRun): Seq[Event] = {
    val feeds = rx.get("feeds").get.asInstanceOf[Seq[NewsFeed]]
    val seenPosts = rx.get("seen_posts").get.asInstanceOf[Set[String]]
    val max = rx.get("max").asInstanceOf[Option[Int]].getOrElse(DetectorNews.DEF_MAX)
    val maxSeenPosts = rx.get("max_seen_posts").asInstanceOf[Option[Int]].getOrElse(DetectorNews.DEF_MAX_SEEN_POSTS)

    var errorEvents = Seq.empty[Event]

    log.info(s"${rx.getExtId()}: Feed: ${feeds}")

    // Fetch all posts from all feeds, sorting and applying max limit per feed
    val ts0 = System.currentTimeMillis()
    val allPosts = feeds.flatMap { feed =>
      feed.fetchFeed() match {
        case Success(posts) =>
          // Sort posts by publishedDate in descending order (newest first)
          val sortedPosts = posts.sortBy(-_.publishedDate)
          
          // Apply max limit to this feed (0 = no limit)
          val limitedPosts = if (max > 0) sortedPosts.take(max) else sortedPosts
          log.info(s"${rx.getExtId()}: Feed: ${feed.getSource()}: ${posts.size} (fetched), ${limitedPosts.size} (max)")
          limitedPosts
        case Failure(e) =>
          log.warn(s"${rx.getExtId()}: Failed to fetch from ${feed.getSource()}: ${e.getMessage}")
          errorEvents = errorEvents ++ handleFeedError(rx, feed, e, System.currentTimeMillis() - ts0)
          Seq.empty
      }
    }

    val ts1 = System.currentTimeMillis()
    val latency = ts1 - ts0

    // Filter for new posts
    val newPosts = allPosts.filterNot(p => seenPosts.contains(p.id))

    // Apply script filters if configured
    val filteredPosts = filterByScripts(rx, newPosts)

    log.info(s"${rx.getExtId()}: Posts: ${allPosts.size} (all), ${seenPosts.size} (seen), ${newPosts.size} (new), ${filteredPosts.size} (filtered)")

    // Update seen posts with size limit (only track posts that were processed)
    val processedPostIds = allPosts.map(_.id).toSet
    val updatedSeen = (seenPosts ++ processedPostIds).takeRight(maxSeenPosts)
    rx.set("seen_posts", updatedSeen)

    // Generate alerts for new filtered posts
    val postEvents = filteredPosts.map(post => createPostAlert(rx, post, latency))

    errorEvents ++ postEvents
  }

  def filterByScripts(rx: SentryRun, posts: Seq[NewsPost]): Seq[NewsPost] = {
    val scriptsOpt = rx.get("scripts").asInstanceOf[Option[ScriptFlow]]

    // If no scripts configured, match everything
    if (! scriptsOpt.isDefined) return posts

    val scripts = scriptsOpt.get
    val threshold = rx.get("threshold").get.asInstanceOf[ThresholdDouble]

    posts.filter { post =>
      val searchText = s"${post.title} ${post.summary}"

      scripts.run("", searchText, Map.empty) match {
        case Success(result) =>
          // Try to parse result as Double
          Try(result.toDouble) match {
            case Success(score) =>
              // Check if score meets threshold condition
              threshold.set(score)
            case Failure(_) =>
              // Could not parse as Double, skip this element
              log.warn(s"${rx.getExtId()}: Could not parse script result as Double: '${result}': post=${post.id}")
              false
          }
        case Failure(e) =>
          // Script execution failed
          log.warn(s"${rx.getExtId()}: Script execution failed: post=${post.id}: ${e.getMessage}")
          false
      }
    }
  }

  private def createPostAlert(rx: SentryRun, post: NewsPost, latency: Long): Event = {
    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorNews.DEF_DESC)

    val metadata = Map(
      "type" -> post.typ,
      "id" -> post.id,
      "title" -> post.title,
      "link" -> post.link,
      "author" -> post.author,
      "date" -> DateParser.formatTimestamp(post.publishedDate),
      "summary" -> post.summary,
      "src" -> post.source,
      "latency" -> latency.toString,
      "desc" -> desc.replace("{title}", post.title),
      "tx_hash" -> post.id
    ) ++ post.feedMetadata

    EventUtil.createEvent(
      did,
      tx = None,
      monitoredAddr = None,
      conf = Some(rx.getConf()),
      meta = metadata,
      detectorTs = post.publishedDate.toString,
      eid0 = eid(post),
      sev = Some(DetectorNews.DEF_SEV_NEW_POST)
    )
  }

  private def handleFeedError(rx: SentryRun, feed: NewsFeed, error: Throwable,latency: Long): Seq[Event] = {
    val trackErr = rx.get("track_err").asInstanceOf[Option[Boolean]].getOrElse(DetectorNews.DEF_TRACK_ERR)

    if (!trackErr) return Seq.empty

    val always = rx.get("err_always").asInstanceOf[Option[Boolean]].getOrElse(DetectorNews.DEF_TRACK_ERR_ALWAYS)
    val errorKey = s"err_last_${feed.getSourceType()}_${feed.getSource().hashCode}"
    val lastErr = rx.get(errorKey).asInstanceOf[Option[String]]
    val errMsg = error.getMessage()

    if (always || lastErr.isEmpty || lastErr.get != errMsg) {
      rx.set(errorKey, errMsg)
      val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorNews.DEF_DESC)

      Seq(EventUtil.createEvent(
        did,
        tx = None,
        monitoredAddr = None,
        conf = Some(rx.getConf()),
        meta = Map(
          "err" -> errMsg,
          "type" -> feed.getSourceType(),
          "src" -> feed.getSource(),
          "latency" -> latency.toString,
          "desc" -> desc
        ),
        sev = Some(DetectorNews.DEF_SEV_ERR)
      ))
    } else {
      Seq.empty
    }
  }
}
