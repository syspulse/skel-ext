package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Await, ExecutionContext, Future}
import java.util.concurrent.TimeUnit

import io.syspulse.skel.plugin.{Plugin, PluginDescriptor}
import io.syspulse.skel.util.Util
import io.syspulse.skel.script.{Script, ScriptFlow}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.sentinel.Config
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.core.Event
import io.hacken.ext.sentinel.ScriptEngine
import io.hacken.ext.sentinel.ThresholdDouble

import io.syspulse.ext.sentinel.feeds._

object DetectorFeed {
  val DEF_CRON = "10 minutes"
  val DEF_FEEDS = ""
  val DEF_TYPE = ""  // Empty = parse URI prefixes (rss://, reddit://, twitter://, meltwater://), "rss" = all RSS, "reddit" = all Reddit, "twitter" = all Twitter, "meltwater" = all Meltwater
  val DEF_DESC = "{title}{err}"
  val DEF_MAX = 0  // 0 = no limit on posts to parse
  val DEF_MAX_SEEN_POSTS = 100
  val DEF_SCORE = ""  // Default threshold for score-based filtering
  val DEF_CATEGORY = None  // Empty = all categories

  val DEF_TRACK_ERR = true
  val DEF_TRACK_ERR_ALWAYS = true

  val DEF_SEV_NEW_POST = Severity.INFO
  val DEF_SEV_ERR = Severity.ERROR

  val DEF_EID_HASH = true

  val DEF_TIMEOUT = 10000L
  val DEF_DELAY = 0L

  /**
   * Parse feed URI based on type configuration
   * @param uri The feed URI (may have rss://, reddit://, twitter://, or meltwater:// prefix)
   * @param feedType The configured feed type ("", "rss", "atom", "reddit", "twitter", or "meltwater")
   * @return Tuple of (actualFeedType: "rss", "atom", "reddit", "twitter", or "meltwater", cleanedUri: String)
   */
  def parseFeedUri(uri: String, feedType: String): (String, String) = {
    // Prefix ALWAYS overrides configured type.
    // Also: we strip the prefix for concrete Feed implementations where possible,
    // except twitter:// which is parsed by TwitterConnect.
    if (uri.startsWith("rss://")) {
      ("rss", uri.substring(6))
    } else if (uri.startsWith("atom://")) {
      ("atom", uri.substring(7))
    } else if (uri.startsWith("reddit://")) {
      ("reddit", uri.substring(9))
    } else if (uri.startsWith("twitter://")) {
      ("twitter", uri) // keep full URI for TwitterConnect
    } else if (uri.startsWith("meltwater://")) {
      ("meltwater", uri) // keep full URI so MeltwaterFeed can detect API mode
    } else if (uri.startsWith("csv://")) {
      ("meltwater", uri) // csv:// is a Meltwater CSV payload; keep prefix so the feed parses CSV
    } else {
      feedType.toLowerCase match {
        case "rss" => ("rss", uri)
        case "atom" => ("atom", uri)
        case "reddit" => ("reddit", uri)
        case "twitter" => ("twitter", uri)
        case "meltwater" => ("meltwater", uri)
        case "" => ("rss", uri) // default
        case _ => ("rss", uri) // unknown -> default
      }
    }
  }

  /**
   * Check if a news post matches the configured category filter
   * @param post The news post to check
   * @param categories Optional sequence of category filters (empty = all categories)
   * @return true if post matches the category filter
   */
  def isCategory(post: NewsPost, categories: Option[Seq[String]]): Boolean = {
    if (!categories.isDefined || categories.get.isEmpty)
      return true

    val categories1 = categories.get.map(_.trim).filter(!_.isBlank)
    val positiveCategories = categories1.filter(!_.startsWith("!"))
    val negativeCategories = categories1.filter(_.startsWith("!"))

    // Positive match: at least one positive category must match (OR logic)
    val positiveMatch = if (positiveCategories.isEmpty) true
                        else positiveCategories.exists(c => post.categories.contains(c))

    // Negative match: no negative categories should match (AND logic)
    val negativeMatch = if (negativeCategories.isEmpty) true
                        else negativeCategories.forall(c => !post.categories.contains(c.substring(1).trim))

    positiveMatch && negativeMatch
  }
}

class DetectorFeed(pd: PluginDescriptor) extends Sentry0 with Plugin {
  override def did = pd.name
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  private implicit val ec: ExecutionContext = ExecutionContext.global

  def eid(rx: SentryRun0,post: NewsPost) = {
    val eid0 = s"${rx.getExtId('_')}_${post.id}"
    if (DetectorFeed.DEF_EID_HASH) {
      Some(Util.sha256(eid0))
    } else {
      Some(eid0)
    }
  }

  override def getSettings(rx: SentryRun0): Map[String, Any] = {
    rx.getConfig().env match {
      case "test" => Map()
      case "dev" =>
        Map("_cron_rate_limit" -> 1 * 60 * 1000L) // 1 min for dev
      case _ =>
        Map("_cron_rate_limit" -> 10 * 60 * 1000L) // 10 min for prod
    }
  }

  override def onInit(rx: SentryRun0, conf: DetectorConfig): Int = {
    val r = super.onInit(rx, conf)
    if (r != SentryRun.SENTRY_INIT) {
      return r
    }

    // Initialize seen posts set
    rx.set("seen_posts", Set.empty[String])

    SentryRun.SENTRY_INIT
  }

  override def onStart(rx: SentryRun0, conf: DetectorConfig): Int = {
    val r = onUpdate(rx, conf)
    if (r != SentryRun.SENTRY_RUNNING) {
      return r
    }

    super.onStart(rx, conf)
  }

  override def onUpdate(rx: SentryRun0, conf: DetectorConfig): Int = {
    // Parse feeds configuration
    val feedsStr = DetectorConfig.getString(conf, "feeds", DetectorFeed.DEF_FEEDS)
    if (feedsStr.isEmpty) {
      log.warn(s"${rx.getExtId()}: feeds configuration required")
      error("Feeds configuration required", None)
      return SentryRun.SENTRY_STOPPED
    }

    val max = DetectorConfig.getInt(conf, "max", DetectorFeed.DEF_MAX)
    val maxSeenPosts = DetectorConfig.getInt(conf, "max_seen_posts", DetectorFeed.DEF_MAX_SEEN_POSTS)
    rx.set("max",max)
    rx.set("max_seen_posts", maxSeenPosts)
    rx.set("desc", DetectorConfig.getString(conf, "desc", DetectorFeed.DEF_DESC))

    rx.set("event_type", DetectorConfig.getString(conf, "event_type"))
    rx.set("event_cat", DetectorConfig.getString(conf, "event_cat"))
    rx.set("event_sid", DetectorConfig.getString(conf, "event_sid"))

    // Get feed type configuration
    val feedType = DetectorConfig.getString(conf, "type", DetectorFeed.DEF_TYPE).toLowerCase

    // Create feed instances based on type configuration
    val feeds: Seq[NewsFeed] = scala.collection.immutable.ArraySeq
      .unsafeWrapArray(feedsStr.split(","))
      .map(_.trim)
      .filter(!_.isBlank)
      .map { uri =>
        val (actualFeedType, cleanedUri) = DetectorFeed.parseFeedUri(uri, feedType)
        actualFeedType match {
          case "rss" => new RssFeed(cleanedUri)
          case "atom" => new AtomFeed(cleanedUri)
          case "reddit" => new RedditFeed(cleanedUri)
          case "twitter" => new TwitterFeed(cleanedUri,Some(max))
          case "meltwater" => new MeltwaterFeed(cleanedUri)
          case _ => new RssFeed(cleanedUri) // Fallback
        }
      }

    log.info(s"${rx.getExtId()}: Configured feeds: ${feeds.size} (${feeds})")
    rx.set("feeds", feeds)

    // Load scripts and error tracking configuration using ScriptEngine
    ScriptEngine.loadConfig(rx, conf, DetectorFeed.DEF_TRACK_ERR, DetectorFeed.DEF_TRACK_ERR_ALWAYS)

    // Set threshold for score-based filtering (optional)
    val scoreCondition = DetectorConfig.getString(conf, "score", DetectorFeed.DEF_SCORE)
    val score = new ThresholdDouble(0.0, scoreCondition)
    rx.set("score", score)

    // Load categories filter configuration
    val categories = DetectorConfig.getString(conf, "categories")
      .orElse(DetectorFeed.DEF_CATEGORY)
      .map(_.split(",").map(_.trim).filter(!_.isBlank).toSeq)
    rx.set("category", categories)
    log.info(s"${rx.getExtId()}: Categories filter: ${categories.getOrElse(Seq.empty)}")

    rx.set("timeout", DetectorConfig.getLong(conf, "timeout", DetectorFeed.DEF_TIMEOUT))
    rx.set("delay", DetectorConfig.getLong(conf, "delay").map(_.longValue()))

    // Initialize seen posts with current feed state (don't alert on first run)
    //initializeSeenPosts(rx)

    SentryRun.SENTRY_RUNNING
  }

  // private def initializeSeenPosts(rx: SentryRun0): Unit = {
  //   val feeds = rx.get("feeds").get.asInstanceOf[Seq[NewsFeed]]

  //   implicit val cfg: Config = rx.getConfig()
  //   val timeout = rx.get("timeout").asInstanceOf[Option[Long]].getOrElse(DetectorFeed.DEF_TIMEOUT)

  //   val futures = feeds.map { feed =>
  //     feed.fetchFeed(timeout)(ec)
  //       .map(posts => Right(posts))
  //       .recover { case e =>
  //         log.warn(s"${rx.getExtId()}: Failed to initialize: ${feed.getSource()}: ${e.getMessage}")
  //         Left(e)
  //       }
  //   }
    
  //   // wait for all feeds to be initialized in parallel with some buffer
  //   val wait = Duration(timeout + 1000L, TimeUnit.MILLISECONDS)

  //   val results = Await.result(Future.sequence(futures), wait)
  //   val currentPosts = results.flatMap {
  //     case Right(posts) => posts
  //     case Left(_) => Seq.empty
  //   }

  //   val postIds = currentPosts.map(_.id).toSet
  //   rx.set("seen_posts", postIds)
  //   log.info(s"${rx.getExtId()}: Initialized: ${postIds.size} (seen posts)")
  // }

  override def onCron(rx: SentryRun0, elapsed: Long): Seq[Event] = {
    checkFeeds(rx)
  }

  // override def onCronAsync(rx: SentryRun0, elapsed: Long): Future[Seq[Event]] = {
  //   Future.successful(checkFeeds(rx))
  // }

  def checkFeeds(rx: SentryRun0): Seq[Event] = {
    val feeds = rx.get("feeds").get.asInstanceOf[Seq[NewsFeed]]
    val seenPosts = rx.get("seen_posts").get.asInstanceOf[Set[String]]
    val max = rx.get("max").asInstanceOf[Option[Int]].getOrElse(DetectorFeed.DEF_MAX)
    val maxSeenPosts = rx.get("max_seen_posts").asInstanceOf[Option[Int]].getOrElse(DetectorFeed.DEF_MAX_SEEN_POSTS)

    var errorEvents = Seq.empty[Event]

    log.info(s"${rx.getExtId()}: Feed: ${feeds}")

    // Fetch all posts from all feeds, sorting and applying max limit per feed
    val ts0 = System.currentTimeMillis()
    //implicit val cfg: Config = rx.getConfig()
    
    val timeout = rx.get("timeout").asInstanceOf[Option[Long]].getOrElse(DetectorFeed.DEF_TIMEOUT)
    val delay = rx.get("delay").asInstanceOf[Option[Option[Long]]].flatten
    val feedWait = Duration(timeout + 1000L, TimeUnit.MILLISECONDS)

    val fetched = delay match {
      case Some(delay) if delay > 0 =>        
        feeds.zipWithIndex.map { case (feed, idx) =>
          if (idx > 0) Thread.sleep(delay)
          try {
            log.info(s"${rx.getExtId()}: Fetching: '${feed.getSource()}'")
            val posts = Await.result(feed.fetchFeed(timeout)(ec), feedWait)
            (feed, Success(posts): Try[Seq[NewsPost]])
          } catch {
            case e: Throwable => (feed, Failure(e): Try[Seq[NewsPost]])
          }
        }
      case _ =>
        val futures = feeds.map { feed =>
          log.info(s"${rx.getExtId()}: Fetching: '${feed.getSource()}'")
          feed.fetchFeed(timeout)(ec)
            .map(posts => (feed, Success(posts): Try[Seq[NewsPost]]))
            .recover { case e => (feed, Failure(e): Try[Seq[NewsPost]]) }
        }
        Await.result(Future.sequence(futures), feedWait)
    }

    val allPosts = fetched.flatMap { case (feed, r) =>
      r match {
        case Success(posts) =>
          val sortedPosts = posts.sortBy(-_.publishedDate)
          val limitedPosts = if (max > 0) sortedPosts.take(max) else sortedPosts
          log.info(s"${rx.getExtId()}: Feed: ${feed.getSource()}: ${posts.size} (fetched), ${limitedPosts.size} (max)")
          limitedPosts
        case Failure(e) =>
          log.warn(s"${rx.getExtId()}: Failed to fetch from '${feed.getSource()}': ${e.getMessage}",e)
          errorEvents = errorEvents ++ handleFeedError(rx, feed, e, System.currentTimeMillis() - ts0)
          Seq.empty
      }
    }
    
    // Filter for new posts
    val newPosts = allPosts.filterNot(p => seenPosts.contains(p.id))

    // Apply script filters if configured
    val scriptFilteredPosts = filter(rx, newPosts)

    // Apply category filters if configured
    val categories = rx.get("category").asInstanceOf[Option[Option[Seq[String]]]].flatten
    val filteredPosts = scriptFilteredPosts.filter(post => DetectorFeed.isCategory(post, categories))

    log.info(s"${rx.getExtId()}: Posts: ${allPosts.size} (all), ${seenPosts.size} (seen), ${newPosts.size} (new), ${scriptFilteredPosts.size} (script-filtered), ${filteredPosts.size} (filtered)")

    // Update seen posts with size limit (only track posts that were processed)
    val processedPostIds = allPosts.map(_.id).toSet
    val updatedSeen = (seenPosts ++ processedPostIds).takeRight(maxSeenPosts)
    rx.set("seen_posts", updatedSeen)

    val ts1 = System.currentTimeMillis()
    val latency = ts1 - ts0

    // Generate alerts for new filtered posts
    val postEvents = filteredPosts.map(post => createPostAlert(rx, post, latency))

    errorEvents ++ postEvents
  }

  def filter(rx: SentryRun0, posts: Seq[NewsPost]): Seq[NewsPost] = {
    val scriptsOpt = rx.get("scripts").asInstanceOf[Option[ScriptFlow]]
    
    // If no scripts configured, match everything
    if (! scriptsOpt.isDefined) return posts
    
    val scripts = scriptsOpt.get

    // If no scripts configured, match everything
    if (scripts.size() == 0) return posts
    
    val scoreOpt = rx.get("score").asInstanceOf[Option[ThresholdDouble]]

    posts.flatMap { post =>
      val scriptText = s"${post.title} ${post.summary}"
      // Pass author and images to script
      val args = Map(
        "author" -> post.author,
        "images" -> post.images.mkString(",")
      )

      val r = scripts.run("", scriptText, args)

      log.debug(s"${rx.getExtId()}: ${post.id}/${post.source}: ${scripts}: score=${scoreOpt}, r=${r}")
            
      r match {
        case Success(result) if(scoreOpt.isDefined && !scoreOpt.get.getCondition.isBlank) =>
          val condition = scoreOpt.get

          // Try to parse result as Double
          Try(result.toDouble) match {
            case Success(score) =>

              // Check if score meets threshold condition
              val r = if(condition.set(score))
                Some(post.copy(result = Map("score" -> score.toString)))
              else
                None

              log.info(s"${rx.getExtId()}: FeedData(${post.id},${post.source},${post.title}): ${scripts} / score=${score}: r=${r.map(_.id)}")
              r

            case Failure(e) =>
              // Could not parse as Double, skip this element
              log.warn(s"${rx.getExtId()}: Failed to parse result: ${post.id}/${post.source}: ${e.getMessage}: '${result}'")
              None
          }

        // no threshold or empty condition means with result -> return Post with extended info
        // ATTENTION: also it checks for non-blank results which may include spaces !!!
        case Success(result) if(result.nonEmpty) =>
          Some(post.copy(result = Map("result" ->result)))

        // blank response ('') and no condition
        case Success(result) =>
          None

        case f @ Failure(e: Script.ScriptBreakException) => 
          log.info(s"${rx.getExtId()}: Script break: ${post.id}/${post.source}: ${e.getMessage}")
          None
        case Failure(e) =>
          // Script execution failed
          log.warn(s"${rx.getExtId()}: Script execution failed: ${post.id}/${post.source}: ${e.getMessage}")
          None
      }
    }
  }

  def sanitize(src0:String): String = {
    // remove after 
    val src = src0.toLowerCase()
    Seq("apikey","api_key")
      .map(k => src.indexOf(k))
      .filter(_ > 0)
      .headOption
      .map(i => src.substring(0,i))
      .getOrElse(src0)
  }

  protected def createPostAlert(rx: SentryRun0, post: NewsPost, latency: Long): Event = {
    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorFeed.DEF_DESC)

    val metadata = Map(
      "type" -> post.typ,
      "id" -> post.id,
      "title" -> post.title,
      "ref" -> post.link,
      "author" -> post.author,
      "date" -> DateParser.formatTimestamp(post.publishedDate),
      "summary" -> post.summary,
      "src" -> sanitize(post.source),
      "latency" -> latency.toString,
      "desc" -> desc,

      "tx_hash" -> post.id,

      "_did" -> rx.getId().toString,
    ) ++ post.feedMetadata ++ post.result

    val sev = post.result.get("score").map(s => s.toDouble).orElse(Some(DetectorFeed.DEF_SEV_NEW_POST))    

    EventUtil.createEvent0(
      did,
      tx = None,
      monitoredAddr = None,
      conf = Some(rx.getConf()),
      meta = metadata,
      detectorTs = Some(post.publishedDate.toString),
      eid0 = eid(rx,post),
      sev = sev,

      // custom event fiels
      typ = rx.getDataStringOpt("event_type"),
      cat = rx.getDataStringOpt("event_cat"),
      sid = rx.getDataStringOpt("event_sid"),
    )
  }

  private def handleFeedError(rx: SentryRun0, feed: NewsFeed, error: Throwable,latency: Long): Seq[Event] = {
    val trackErr = rx.get("track_err").asInstanceOf[Option[Boolean]].getOrElse(DetectorFeed.DEF_TRACK_ERR)

    if (!trackErr) return Seq.empty

    val always = rx.get("err_always").asInstanceOf[Option[Boolean]].getOrElse(DetectorFeed.DEF_TRACK_ERR_ALWAYS)
    val errorKey = s"err_last_${feed.getSourceType()}_${feed.getSource().hashCode}"
    val lastErr = rx.get(errorKey).asInstanceOf[Option[String]]
    val errMsg = error.getMessage()

    if (always || lastErr.isEmpty || lastErr.get != errMsg) {
      rx.set(errorKey, errMsg)
      val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorFeed.DEF_DESC)
      
      Seq(EventUtil.createEvent0(
        did,
        tx = None,
        monitoredAddr = None,
        conf = Some(rx.getConf()),
        meta = Map(
          "err" -> errMsg,
          "type" -> feed.getSourceType(),
          "src" -> sanitize(feed.getSource()),
          "latency" -> latency.toString,
          "desc" -> desc,
          "did" -> rx.getId().toString,
        ),
        sev = Some(DetectorFeed.DEF_SEV_ERR),
        
        // custom event fiels
        typ = rx.getDataStringOpt("event_type"),
        cat = rx.getDataStringOpt("event_cat"),
        sid = rx.getDataStringOpt("event_sid"),

      ))
    } else {
      Seq.empty
    }
  }
}
