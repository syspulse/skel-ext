package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try, Success, Failure}

import io.syspulse.skel.plugin.{Plugin, PluginDescriptor}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.core.Event

import io.syspulse.ext.sentinel.feeds._
import io.syspulse.skel.util.Util
import io.syspulse.skel.script.{Script, ScriptFlow}
import io.syspulse.skel.util.ConditionDouble

import io.hacken.ext.sentinel.ScriptEngine

object DetectorMeltwater {
  val DEF_SOURCE = ""  // Filter by Source field (comma-separated list)
  val DEF_INFLUENCER = ""  // Filter by Influencer field (comma-separated list)
  val DEF_COUNTRY = ""  // Filter by Country field (comma-separated list)
  val DEF_LANGUAGE = ""  // Filter by Language field (comma-separated list)
  val DEF_REACH = ""  // Filter by Reach field (threshold condition, e.g., ">1000")
  val DEF_SOCIAL_ECHO = ""  // Filter by total Social Echo (threshold condition)
  val DEF_DOCUMENT_TAGS = ""  // Filter by Document Tags field (comma-separated list)

  private def parseListFilter(s: String): Seq[String] =
    s.trim.split(",").iterator.map(_.trim).filter(!_.isBlank).toSeq

  /**
   * Check if a news post matches the configured Meltwater filters
   */
  def matchesFilters(
    post: NewsPost,
    sourceFilter: Seq[String],
    influencerFilter: Seq[String],
    countryFilter: Seq[String],
    languageFilter: Seq[String],
    reachThreshold: Option[ConditionDouble],
    socialEchoThreshold: Option[ConditionDouble],
    documentTagsFilter: Seq[String]
  ): Boolean = {
    // Extract metadata fields
    val source = post.feedMetadata.getOrElse("Source", "")
    val influencer = post.feedMetadata.getOrElse("Influencer", "")
    val country = post.feedMetadata.getOrElse("Country", "")
    val language = post.feedMetadata.getOrElse("Language", "")
    val reach = post.feedMetadata.get("Reach").flatMap(r => Try(r.toDouble).toOption).getOrElse(0.0)
    val documentTags = post.feedMetadata.getOrElse("Document_Tags", "")

    // Calculate total social echo (sum of Twitter, Facebook, Reddit)
    val twitterEcho = post.feedMetadata.get("Twitter_Social_Echo").flatMap(r => Try(r.toDouble).toOption).getOrElse(0.0)
    val facebookEcho = post.feedMetadata.get("Facebook_Social_Echo").flatMap(r => Try(r.toDouble).toOption).getOrElse(0.0)
    val redditEcho = post.feedMetadata.get("Reddit_Social_Echo").flatMap(r => Try(r.toDouble).toOption).getOrElse(0.0)
    val totalSocialEcho = twitterEcho + facebookEcho + redditEcho

    // Apply filters (all must pass)
    val sourceMatch = sourceFilter.isEmpty || sourceFilter.exists(f => matchesPattern(source, f))
    val influencerMatch = influencerFilter.isEmpty || influencerFilter.exists(f => matchesPattern(influencer, f))
    val countryMatch = countryFilter.isEmpty || countryFilter.exists(f => matchesPattern(country, f))
    val languageMatch = languageFilter.isEmpty || languageFilter.exists(f => matchesPattern(language, f))
    val reachMatch = reachThreshold.isEmpty || reachThreshold.get.set(reach)
    val socialEchoMatch = socialEchoThreshold.isEmpty || socialEchoThreshold.get.set(totalSocialEcho)
    val documentTagsMatch = documentTagsFilter.isEmpty || documentTagsFilter.exists(f => matchesPattern(documentTags, f))

    sourceMatch && influencerMatch && countryMatch && languageMatch && reachMatch && socialEchoMatch && documentTagsMatch
  }

  private def matchesPattern(value: String, pattern: String): Boolean = {
    if (pattern.trim.startsWith("!")) {
      // Negative match
      !value.toLowerCase.contains(pattern.substring(1).toLowerCase)
    } else {
      // Positive match (contains or equals)
      value.toLowerCase.contains(pattern.toLowerCase)
    }
  }
}

class DetectorMeltwater(pd: PluginDescriptor) extends DetectorFeed(pd) {

  override def onUpdate(rx: SentryRun0, conf: DetectorConfig): Int = {
    val r = super.onUpdate(rx, conf)
    if (r != SentryRun.SENTRY_RUNNING) {
      return r
    }

    val apiKey = DetectorConfig.getString(conf, "api_key","")
    rx.set("api_key", apiKey)

    // Build the Search API query parameters (rolling window + paging).
    val query = MeltwaterQuery(
      windowMs = MeltwaterFeed.parseWindow(
        DetectorConfig.getString(conf, "window", ""), MeltwaterFeed.DEF_WINDOW_MS),
      pageFrom = DetectorConfig.getInt(conf, "page_from", MeltwaterFeed.DEF_PAGE_FROM),
      pageSize = DetectorConfig.getInt(conf, "page_size", MeltwaterFeed.DEF_PAGE_SIZE),
      sortBy = DetectorConfig.getString(conf, "sort_by", MeltwaterFeed.DEF_SORT_BY),
      sortOrder = DetectorConfig.getString(conf, "sort_order", MeltwaterFeed.DEF_SORT_ORDER),
      tz = DetectorConfig.getString(conf, "tz", MeltwaterFeed.DEF_TZ),
      template = DetectorConfig.getString(conf, "template", MeltwaterFeed.DEF_TEMPLATE)
    )

    // Override feed type to "meltwater" and recreate feeds as MeltwaterFeed instances.
    // Keep the URI prefix intact (meltwater:// / csv:// / file://) so the feed can pick its format.
    val feedsStr = DetectorConfig.getString(conf, "feeds", DetectorFeed.DEF_FEEDS)
    if (feedsStr.isEmpty) {
      log.warn(s"${rx.getExtId()}: feeds configuration required")
      error("Feeds configuration required", None)
      return SentryRun.SENTRY_STOPPED
    }

    val feeds: Seq[NewsFeed] = scala.collection.immutable.ArraySeq
      .unsafeWrapArray(feedsStr.split(","))
      .map(_.trim)
      .filter(!_.isBlank)
      .map { uri =>
        new MeltwaterFeed(uri, apiKey, query)
      }

    log.info(s"${rx.getExtId()}: Configured Meltwater feeds: ${feeds.size} (${feeds})")
    rx.set("feeds", feeds)

    // Load Meltwater-specific filter configurations
    val sourceFilter =
      DetectorMeltwater.parseListFilter(DetectorConfig.getString(conf, "source", DetectorMeltwater.DEF_SOURCE))
    rx.set("source", sourceFilter)

    val influencerFilter =
      DetectorMeltwater.parseListFilter(DetectorConfig.getString(conf, "influencer", DetectorMeltwater.DEF_INFLUENCER))
    rx.set("influencer", influencerFilter)

    val countryFilter =
      DetectorMeltwater.parseListFilter(DetectorConfig.getString(conf, "country", DetectorMeltwater.DEF_COUNTRY))
    rx.set("country", countryFilter)

    val languageFilter =
      DetectorMeltwater.parseListFilter(DetectorConfig.getString(conf, "language", DetectorMeltwater.DEF_LANGUAGE))
    rx.set("language", languageFilter)

    val reachCondition = DetectorConfig.getString(conf, "reach", DetectorMeltwater.DEF_REACH)
    val reachThreshold = if (!reachCondition.isBlank) Some(new ConditionDouble(0.0, reachCondition)) else None
    rx.set("reach", reachThreshold)

    val socialEchoCondition = DetectorConfig.getString(conf, "social_echo", DetectorMeltwater.DEF_SOCIAL_ECHO)
    val socialEchoThreshold = if (!socialEchoCondition.isBlank) Some(new ConditionDouble(0.0, socialEchoCondition)) else None
    rx.set("social_echo", socialEchoThreshold)

    val documentTagsFilter =
      DetectorMeltwater.parseListFilter(DetectorConfig.getString(conf, "document_tags", DetectorMeltwater.DEF_DOCUMENT_TAGS))
    rx.set("document_tags", documentTagsFilter)

    log.info(s"${rx.getExtId()}: Meltwater filters - source=$sourceFilter, influencer=$influencerFilter, country=$countryFilter, language=$languageFilter, reach=$reachCondition, socialEcho=$socialEchoCondition, tags=$documentTagsFilter")

    SentryRun.SENTRY_RUNNING
  }

  override def checkFeeds(rx: SentryRun0): Seq[Event] = {
    // Call parent to get initial filtered posts
    val events = super.checkFeeds(rx)

    // No additional filtering needed - it's done in filter() override
    events
  }

  override def filter(rx: SentryRun0, posts: Seq[NewsPost]): Seq[NewsPost] = {
    // First apply parent script filters
    val scriptFiltered = super.filter(rx, posts)

    // Then apply Meltwater-specific filters
    val sourceFilter = rx.get("source").asInstanceOf[Option[Seq[String]]].getOrElse(Seq.empty)
    val influencerFilter = rx.get("influencer").asInstanceOf[Option[Seq[String]]].getOrElse(Seq.empty)
    val countryFilter = rx.get("country").asInstanceOf[Option[Seq[String]]].getOrElse(Seq.empty)
    val languageFilter = rx.get("language").asInstanceOf[Option[Seq[String]]].getOrElse(Seq.empty)
    val reachThreshold = rx.get("reach").asInstanceOf[Option[Option[ConditionDouble]]].flatten
    val socialEchoThreshold = rx.get("social_echo").asInstanceOf[Option[Option[ConditionDouble]]].flatten
    val documentTagsFilter = rx.get("document_tags").asInstanceOf[Option[Seq[String]]].getOrElse(Seq.empty)

    val meltwaterFiltered = scriptFiltered.filter { post =>
      DetectorMeltwater.matchesFilters(
        post,
        sourceFilter,
        influencerFilter,
        countryFilter,
        languageFilter,
        reachThreshold,
        socialEchoThreshold,
        documentTagsFilter
      )
    }

    log.info(s"${rx.getExtId()}: Meltwater filter: ${posts.size} -> ${scriptFiltered.size} -> ${meltwaterFiltered.size} posts")

    meltwaterFiltered
  }
}
