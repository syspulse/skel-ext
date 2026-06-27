package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try, Success, Failure}

import io.syspulse.skel.plugin.{Plugin, PluginDescriptor}
import io.syspulse.skel.blockchain.Blockchain

import io.haas.ingest.eth.{Block,Tx}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.core.Event
import io.hacken.ext.sentinel.ThresholdDouble
import io.hacken.ext.sentinel.ThresholdLong

// --------------------------------------------------------------------------------------------------------------------------
object DetectorBubblemaps {
  val DEF_CRON = "1 hour"
  val DEF_CONDITION = "> 0.0"
  val DEF_CLUSTER_SIZE = ""
  val DEF_CLUSTER_SHARE = ""
  val DEF_DESC = "Decentralization: {score}"
  val DEF_TRACK_HOLDERS = 0
  val DEF_TRACK_UPDATE = false

  val DEF_TRACK_ERR = true
  val DEF_TRACK_ERR_ALWAYS = true

  val DEF_SEV_OK = Severity.INFO
  val DEF_SEV_ERR = Severity.ERROR
}

class DetectorBubblemaps(pd: PluginDescriptor) extends SentryEth with Plugin {
  override def did = pd.name
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  override def getSettings(rx: SentryRunEth): Map[String, Any] = {
    rx.getConfig().env match {
      case "test" => Map()
      case "dev" =>
        Map("_cron_rate_limit" -> 10 * 10 * 1000L) // 10 min
      case _ =>
        Map("_cron_rate_limit" -> 60 * 60 * 1000L) // 1 hour
    }
  }

  override def onInit(rx: SentryRunEth, conf: DetectorConfig): Int = {
    val r = super.onInit(rx, conf)
    if (r != SentryRun.SENTRY_INIT) {
      return r
    }

    SentryRun.SENTRY_INIT
  }

  override def onStart(rx: SentryRunEth, conf: DetectorConfig): Int = {
    val r = onUpdate(rx, conf)
    if (r != SentryRun.SENTRY_RUNNING) {
      return r
    }

    super.onStart(rx, conf)
  }

  def mapSeverity(score: Double): Option[Double] = {
    score match {
      case s if s < 10.0 => Some(Severity.CRITICAL)
      case s if s < 30.0 => Some(Severity.HIGH)  // Major
      case s if s < 50.0 => Some(Severity.MEDIUM)
      case s if s >= 65.0 => Some(Severity.LOW)
      case _ => Some(Severity.MEDIUM)  // 50.0 - 64.9 range
    }
  }

  override def onUpdate(rx: SentryRunEth, conf: DetectorConfig): Int = {
    if (rx.isAddrEmpty()) {
      log.warn(s"${rx.getExtId()}: address undefined: ${rx.getAddr()}")
      error(s"Address undefined",None)
      return SentryRun.SENTRY_STOPPED
    }

    // Get API key from user config or from configuration
    val apiKey = {
      val key = DetectorConfig.getString(conf, "api_key", "")
      if (key.isEmpty()) {
        rx.getConfiguration()(c => c.getString("bm.api.key")).getOrElse("")
      } else {
        key
      }
    }

    if (apiKey.isEmpty()) {
      log.warn(s"${rx.getExtId()}: Bubblemaps API Key is required")
      error(s"Bubblemaps API Key is required",None)
      return SentryRun.SENTRY_STOPPED
    }

    // Create Bubblemaps instance with apiKey
    val bubblemaps = new Bubblemaps(apiKey)
    rx.set("bubblemaps", bubblemaps)

    // Set threshold condition
    val condition = DetectorConfig.getString(conf, "condition", DetectorBubblemaps.DEF_CONDITION)
    val threshold = new ThresholdDouble(0.0, condition)
    rx.set("threshold", threshold)

    // Set cluster thresholds
    val clusterSize = DetectorConfig.getString(conf, "cluster_size", DetectorBubblemaps.DEF_CLUSTER_SIZE)
    rx.set("cluster_size", new ThresholdLong(0, clusterSize))

    val clusterShare = DetectorConfig.getString(conf, "cluster_share", DetectorBubblemaps.DEF_CLUSTER_SHARE)
    rx.set("cluster_share", new ThresholdDouble(0.0, clusterShare))

    // Set track_holder parameter
    val trackHolder = DetectorConfig.getInt(conf, "track_holders", DetectorBubblemaps.DEF_TRACK_HOLDERS)
    rx.set("track_holders", trackHolder)

    // Set track_update parameter
    val trackUpdate = DetectorConfig.getBoolean(conf, "track_update", DetectorBubblemaps.DEF_TRACK_UPDATE)
    rx.set("track_update", trackUpdate)

    // Set description
    rx.set("desc", DetectorConfig.getString(conf, "desc", DetectorBubblemaps.DEF_DESC))

    // Set error tracking options
    rx.set("track_err", DetectorConfig.getBoolean(conf, "track_err", DetectorBubblemaps.DEF_TRACK_ERR))
    rx.set("err_always", DetectorConfig.getBoolean(conf, "err_always", DetectorBubblemaps.DEF_TRACK_ERR_ALWAYS))

    // Initialize threshold with first query
    // checkDecentralization(rx, init = true)

    SentryRun.SENTRY_RUNNING
  }

  def checkDecentralization(rx: SentryRunEth, init: Boolean = false): Seq[Event] = {
    val addr = rx.getAddr() match {
      case Some(addr) => addr.toLowerCase
      case None =>
        log.warn(s"${rx.getExtId()}: address undefined")
        return Seq.empty
    }

    val chain = rx.getChain().getOrElse("") match {
      case "" =>
        log.warn(s"${rx.getExtId()}: chain undefined")
        return Seq.empty
      case c => c
    }

    val bubblemaps = rx.get("bubblemaps").get.asInstanceOf[Bubblemaps]
    val threshold = rx.get("threshold").get.asInstanceOf[ThresholdDouble]
    val trackHolders = rx.get("track_holders").get.asInstanceOf[Int]
    val trackUpdate = rx.get("track_update").get.asInstanceOf[Boolean]

    val result = bubblemaps.getMapData(addr, chain, withHolders = trackHolders > 0)

    result match {
      case Success(data) =>
        val score = data.decentralization_score
        val oldScore = threshold.value()
        val scoreChanged = threshold.set(score)

        // Get cluster thresholds from context
        val clusterSizeThreshold = rx.get("cluster_size").get.asInstanceOf[ThresholdLong]
        val clusterShareThreshold = rx.get("cluster_share").get.asInstanceOf[ThresholdDouble]

        // Check clusters and collect matches
        val clusterSizeMatches = data.clusters.filter { cluster =>
          clusterSizeThreshold.set(cluster.holder_count)
        }

        val clusterShareMatches = data.clusters.filter { cluster =>
          clusterShareThreshold.set(cluster.share)
        }

        val clusterSizeChanged = clusterSizeMatches.nonEmpty
        val clusterShareChanged = clusterShareMatches.nonEmpty

        log.info(s"${rx.getExtId()}: addr=${addr}, chain=${chain}: decentralization_score: ${oldScore} -> ${score}, scoreChanged=${scoreChanged}, clusterSizeChanged=${clusterSizeChanged}, clusterShareChanged=${clusterShareChanged}")

        // Trigger alert if ANY condition is met (OR operation)
        if (scoreChanged || clusterSizeChanged || clusterShareChanged) {
          // Map decentralization_score to severity
          val severity = mapSeverity(score)

          // Combine matching clusters (union, deduplicated) and sort by share descending
          val filteredClusters = (clusterSizeMatches ++ clusterShareMatches).distinct.sortBy(-_.share)

          // Get Bubblemaps chain for link
          val bubblemapsChain = bubblemaps.mapChain(chain).getOrElse(chain)
          val link = s"https://v2.bubblemaps.io/map?address=${addr}&chain=${bubblemapsChain}"

          val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorBubblemaps.DEF_DESC)

          // Format holders metadata if track_holder > 0
          val holdersMetadata = if (trackHolders > 0) {
            data.top_holders.take(trackHolders).map { holder =>
              val share = holder.holder_data.map(h => f"${h.share}%.6f").getOrElse("0")
              s"${holder.address}:${share}"
            }.mkString(", ")
          } else {
            ""
          }

          // Base metadata
          val baseMetadata = Map(
            "score" -> score.toString,
            "old" -> oldScore.toString,
            "clusters" -> filteredClusters.length.toString,
            "total" -> data.clusters.length.toString,
            "top" -> holdersMetadata,
            "ref" -> link,
            "desc" -> desc
          )

          // Add cluster JSON objects as separate metadata fields
          import spray.json._
          import BubblemapsJsonProtocol._
          val clusterMetadata = filteredClusters.take(10).zipWithIndex.map { case (cluster, i) =>
            s"cluster_$i" -> cluster.toJson.compactPrint
          }.toMap

          val metadata = baseMetadata ++ clusterMetadata

          // Determine timestamp based on track_update
          val detectorTs = if (trackUpdate) {
            (data.metadata.ts_update * 1000L).toString
          } else {
            "sys"
          }

          Seq(EventUtil.createEvent(
            did,
            tx = None,
            Some(addr),
            conf = Some(rx.getConf()),
            meta = metadata,
            detectorTs = detectorTs,
            sev = severity
          ))
        } else {
          Seq.empty
        }

      case Failure(e) =>
        log.warn(s"${rx.getExtId()}: Failed to fetch Bubblemaps data: ${e.getMessage}")

        val trackErr = rx.get("track_err").asInstanceOf[Option[Boolean]].getOrElse(DetectorBubblemaps.DEF_TRACK_ERR)

        if (trackErr) {
          // Check if error is unique
          val always = rx.get("err_always").asInstanceOf[Option[Boolean]].getOrElse(DetectorBubblemaps.DEF_TRACK_ERR_ALWAYS)
          val lastErr = rx.get("err_last").asInstanceOf[Option[String]]
          val err = e.getMessage()

          if (always || !lastErr.isDefined || lastErr.get != err) {
            rx.set("err_last", err)

            val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorBubblemaps.DEF_DESC)

            Seq(EventUtil.createEvent(
              did,
              tx = None,
              Some(addr),
              conf = Some(rx.getConf()),
              meta = Map(
                "err" -> err,
                "desc" -> desc
              ),
              sev = Some(DetectorBubblemaps.DEF_SEV_ERR)
            ))
          } else {
            Seq.empty
          }
        } else {
          Seq.empty
        }
    }
  }

  override def onCron(rx: SentryRunEth, elapsed: Long): Seq[Event] = {
    checkDecentralization(rx)
  }
}
