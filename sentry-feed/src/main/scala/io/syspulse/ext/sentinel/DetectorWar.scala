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
import io.syspulse.skel.ai.core.AiURI

object DetectorWar {
  // Hardcoded configuration
  val TWITTER_ACCOUNT = "twitter://GeneralStaffUA"
  val DEF_MAX_POSTS = 15
  val DEF_MAX_SEEN_POSTS = 100
  val CRON_INTERVAL = "1 day"

  val DEF_DESC = "Losses Report: {loss_personnel_total}, {loss_aircraft_total}, {loss_UAV_total}, {loss_APC_total}, {loss_MLRS_total}, {loss_SAM_total}, {loss_ship_total}, {loss_submarine_total}, {loss_automotive_fuel_truck_total}, {loss_tank_total}, {loss_special_equipment_total}, {loss_cruise_missile_total}, {loss_artillery_total}, {loss_personnel_total}"
  val DEF_SEV_REPORT = Severity.INFO
  val DEF_SEV_ERR = Severity.ERROR
  val DEF_EID_HASH = true

  val DEF_SCRIPT_REGEXP = ".*Загальні бойові втрати противника.*"
  val DEF_SCRIPT_FILTER = ""
  val DEF_SCRIPT_JS = "'image://' + images.split(',')[0]"
  val DEF_SCRIPT_AI = """Extract text from provided image {input} and return result as json.
  Double check the `loss_personnel_total` OCR value for correctness (you often miss digits like `1` inside the number)
  The `loss_personnel_total` number can NOT be negative or less then 1000000. If it is less than 1000000, you have made incorrect OCR and need to repeat OCR.
  Json attribute rules:
  1. Always use `loss_` prefix. use `_total` suffix for total and `_change` suffix for losses changes.   
  2. Always use the consistent names in attributes: 
    - helicopter, 
    - aircraft,
    - UAV,
    - APC, 
    - MLRS,
    - SAM,
    - ship,
    - submarine,
    - automotive_fuel_truck,
    - tank,
    - special_equipment,
    - personnel,
    - cruise_missile,    
    - artillery,    
    - personnel
  3. Do NOT use random CAPITAL letters in attributes mentioned in rule 2 (e.g. no "ARTILLERY", use "artillery")
  4. Do NOT generate null values for extracted attributes. Use 0 in attributes which have prefix "_change".  
  6. Prefix non-total "losses" values with '+' (negative values are not possible anywhere)

  output://json_object"""

  val DEF_AI_MODEL = "gpt-4o"
  val DEF_AI_TIMEOUT = 60000
  val DEF_AI_URI = s"openai://${DEF_AI_MODEL}?timeout=${DEF_AI_TIMEOUT}"

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
    val max = DetectorConfig.getInt(conf, "max", DetectorWar.DEF_MAX_POSTS)
    val maxSeenPosts = DetectorConfig.getInt(conf, "max_seen_posts", DetectorWar.DEF_MAX_SEEN_POSTS)
    rx.set("max",max)
    rx.set("max_seen_posts", maxSeenPosts)
    rx.set("desc", DetectorConfig.getString(conf, "desc", DetectorWar.DEF_DESC))
    
    // Error tracking - always enabled
    rx.set("track_err", true)
    rx.set("err_always", true)

    // Create hardcoded Twitter feed
    val feed = new TwitterFeed(DetectorWar.TWITTER_ACCOUNT, Some(max))
    rx.set("feeds", Seq(feed))

    val customRegexp = DetectorConfig.getString(conf, "custom_regexp").filter(!_.isBlank).getOrElse(DetectorWar.DEF_SCRIPT_REGEXP)
    val customFilter = DetectorConfig.getString(conf, "custom_filter").filter(!_.isBlank).getOrElse("")
    val customJs = DetectorConfig.getString(conf, "custom_js").filter(!_.isBlank).getOrElse(DetectorWar.DEF_SCRIPT_JS)
    val customAi = DetectorConfig.getString(conf, "custom_ai").filter(!_.isBlank).getOrElse(DetectorWar.DEF_SCRIPT_AI)
    val aiUri = DetectorConfig.getString(conf, "ai_uri").filter(!_.isBlank).getOrElse(DetectorWar.DEF_AI_URI)

    rx.set("ai_uri",AiURI(aiUri))
    
    log.info(s"${rx.getExtId()}: Configured feed: ${feed}")

    // Create hardcoded script flow from direct Script instantiation
    try {
      val scripts: Seq[Script] = Seq(
        new ScriptRegexp(Some(customRegexp)),
        new ScriptFilter(Some(customFilter)),
        new ScriptJS(Some(customJs)),
        new ScriptAI(
          prompt0 = Some(customAi),
          uri0 = Some(aiUri)
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

    SentryRun.SENTRY_RUNNING
  }

  override def createPostAlert(rx: SentryRun, post: NewsPost, latency: Long): Event = {
    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorWar.DEF_DESC)

    val aiUri = rx.get("ai_uri").asInstanceOf[Option[AiURI]]
    val aiModel = aiUri.flatMap(_.model).orElse(Some(DetectorWar.DEF_AI_MODEL))

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
      "model" -> aiModel.getOrElse(""),
      "desc" -> desc,
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
