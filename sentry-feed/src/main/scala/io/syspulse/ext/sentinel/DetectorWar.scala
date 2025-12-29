package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try, Success, Failure}
import spray.json._

import io.syspulse.skel.plugin.{Plugin, PluginDescriptor}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.core.Event

import io.syspulse.ext.sentinel.feeds._
import io.syspulse.skel.util.Util
import io.syspulse.skel.script.{Script, ScriptFlow, ScriptRegexp, ScriptFilter, ScriptJS, ScriptAI}

object DetectorWar {
  // Hardcoded configuration
  val TWITTER_ACCOUNT = "twitter://GeneralStaffUA"
  val MAX_POSTS = 15
  val MAX_SEEN_POSTS = 100
  val CRON_INTERVAL = "150000" // 150 seconds

  val DEF_DESC = "War Report: {title}"
  val DEF_SEV_REPORT = Severity.INFO
  val DEF_SEV_ERR = Severity.ERROR
  val DEF_EID_HASH = true

  /**
   * Flatten nested JSON structure into flat key-value pairs
   * Example: {"losses": {"personnel": {"count": 120569}}} -> {"losses.personnel.count": "120569"}
   */
  def flattenJson(json: JsValue, prefix: String = ""): Map[String, String] = {
    json match {
      case JsObject(fields) =>
        fields.flatMap { case (key, value) =>
          val newPrefix = if (prefix.isEmpty) key else s"$prefix.$key"
          value match {
            case obj: JsObject => flattenJson(obj, newPrefix)
            case arr: JsArray => Map(newPrefix -> arr.compactPrint)
            case other => Map(newPrefix -> other.toString.replaceAll("^\"|\"$", ""))
          }
        }.toMap
      case JsArray(elements) =>
        Map(prefix -> json.compactPrint)
      case other =>
        Map(prefix -> other.toString.replaceAll("^\"|\"$", ""))
    }
  }
}

class DetectorWar(pd: PluginDescriptor) extends DetectorFeed(pd) {
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  override def eid(post: NewsPost) = {
    if (DetectorWar.DEF_EID_HASH) {
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

  override def onUpdate(rx: SentryRun, conf: DetectorConfig): Int = {
    // Hardcoded configuration
    val max = DetectorWar.MAX_POSTS
    val maxSeenPosts = DetectorWar.MAX_SEEN_POSTS
    rx.set("max", max)
    rx.set("max_seen_posts", maxSeenPosts)

    // Create hardcoded Twitter feed
    val feed = new TwitterFeed(DetectorWar.TWITTER_ACCOUNT, Some(max))
    rx.set("feeds", Seq(feed))

    log.info(s"${rx.getExtId()}: Configured feed: ${feed}")

    // Configuration
    rx.set("desc", DetectorWar.DEF_DESC)

    // Create hardcoded script flow from direct Script instantiation
    try {
      val scripts: Seq[Script] = Seq(
        new ScriptRegexp(Some(".*Загальні бойові втрати противника.*")),
        new ScriptFilter(Some("")),
        new ScriptJS(Some("'image://' + images.split(',')[0]")),
        new ScriptAI(
          prompt0 = Some("Extract text from provided image {input} and return result as json. output://json_object"),
          uri0 = Some("openai://?timeout=60000")
        )
      )

      val scriptFlow = new ScriptFlow(scripts)
      rx.set("scripts", scriptFlow)
      log.info(s"${rx.getExtId()}: Loaded ${scripts.size} scripts")
    } catch {
      case e: Exception =>
        log.error(s"${rx.getExtId()}: Failed to load scripts: ${e.getMessage}")
        error(s"Failed to load scripts: ${e.getMessage}", None)
        return SentryRun.SENTRY_STOPPED
    }

    // Error tracking - always enabled
    rx.set("track_err", true)
    rx.set("err_always", true)

    SentryRun.SENTRY_RUNNING
  }

  override def createPostAlert(rx: SentryRun, post: NewsPost, latency: Long): Event = {
    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorWar.DEF_DESC)

    // Base metadata
    var metadata = Map(
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

    // Extract and flatten AI response JSON
    post.result.get("result").foreach { aiResponse =>
      try {
        val jsonResult = aiResponse.parseJson
        val flattenedData = DetectorWar.flattenJson(jsonResult)
        metadata = metadata ++ flattenedData
        log.info(s"${rx.getExtId()}: Extracted ${flattenedData.size} fields from AI response for post ${post.id}")
      } catch {
        case e: Exception =>
          log.warn(s"${rx.getExtId()}: Failed to parse AI response for post ${post.id}: ${e.getMessage}")
          metadata = metadata + ("ai_raw" -> aiResponse)
      }
    }

    EventUtil.createEvent(
      did,
      tx = None,
      monitoredAddr = None,
      conf = Some(rx.getConf()),
      meta = metadata,
      detectorTs = post.publishedDate.toString,
      eid0 = eid(post),
      sev = Some(DetectorWar.DEF_SEV_REPORT)
    )
  }
}
