package io.syspulse.ext.sentinel

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}

import io.syspulse.skel.plugin.{Plugin, PluginDescriptor}

import io.hacken.ext.core.Severity
import io.hacken.ext.core.Event
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.ThresholdDouble
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig

import io.syspulse.ext.sentinel.cloud.{Cloud, CloudAlert}

// --------------------------------------------------------------------------------------------------------------------------
object DetectorCloud {
  val DEF_CRON = "10 min"
  val DEF_URI = "aws://"
  val DEF_SYSTEM = "Watch"                  // comma-separated list: Watch (CloudWatch), Trail (CloudTrail), IAM, Storage (S3)
  val DEF_CONDITION = "> 0"                 // condition on number of (new) findings
  val DEF_DESC = "{system}: {title}{err}"
  val DEF_NEW_ONLY = true                   // alert only on new findings
  val DEF_TIMEOUT = 60000L                  // ms

  val DEF_TRACK_ERR = true
  val DEF_TRACK_ERR_ALWAYS = true

  val DEF_SEV_ERR = Severity.ERROR

  /** Parse systems list. Returns (valid normalized systems, invalid names) */
  def parseSystems(systems: String): (Seq[String], Seq[String]) = {
    val all = systems.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    val parsed = all.map(s => s -> Cloud.system(s))
    (parsed.flatMap(_._2).distinct, parsed.filter(_._2.isEmpty).map(_._1))
  }

  /** Select findings to alert on: only new ones (not seen in the previous check) or all */
  def selectNew(alerts: Seq[CloudAlert], seen: Set[String], newOnly: Boolean): Seq[CloudAlert] = {
    if (newOnly) alerts.filterNot(a => seen.contains(a.id)) else alerts
  }
}

/**
 * Monitors Cloud provider (AWS, GCP, Azure) health and security events: 
 * CloudWatch alarms (VPC, EC2, EKS, ...), CloudTrail events, IAM configuration alerts, Storage (S3) availability
 * 
 * Only `onCron` is supported (no onBlock/onTx)
 * 
 * Configuration:
 *   uri       - Cloud URI: aws://, gcp://, azure:// (see CloudURI)
 *   cron      - check interval
 *   system    - systems to monitor (comma-separated): Watch (CloudWatch), Trail (CloudTrail), IAM, Storage (S3)
 *   condition - condition on number of findings to generate Alerts
 *   new_only  - alert only once on every new finding
 */
class DetectorCloud(pd: PluginDescriptor) extends Sentry0 with Plugin {
  override def did = pd.name
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  override def getSettings(rx: SentryRun0): Map[String, Any] = {
    rx.getConfig().env match {
      case "test" => Map()
      case "dev" =>
        Map("_cron_rate_limit" -> 10 * 1000L)     // 10 sec
      case _ =>
        Map("_cron_rate_limit" -> 60 * 1000L)     // 1 min
    }
  }

  override def onInit(rx: SentryRun0, conf: DetectorConfig): Int = {
    val r = super.onInit(rx, conf)
    if (r != SentryRun.SENTRY_INIT) {
      return r
    }

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
    val desc = DetectorConfig.getString(conf, "desc", DetectorCloud.DEF_DESC)
    rx.set("desc", desc)
    rx.set("track_err", DetectorConfig.getBoolean(conf, "track_err", DetectorCloud.DEF_TRACK_ERR))
    rx.set("err_always", DetectorConfig.getBoolean(conf, "err_always", DetectorCloud.DEF_TRACK_ERR_ALWAYS))

    val (systems, invalid) = DetectorCloud.parseSystems(DetectorConfig.getString(conf, "system", DetectorCloud.DEF_SYSTEM))
    if (invalid.nonEmpty || systems.isEmpty) {
      val err = if (invalid.nonEmpty) s"Invalid system: ${invalid.mkString(",")} (supported: ${Cloud.SYSTEMS.mkString(",")})" else "System undefined"
      log.warn(s"${rx.getExtId()}: ${err}")
      error(desc, Some(err))
      return SentryRun.SENTRY_STOPPED
    }
    rx.set("system", systems)

    // new Cloud is created on every update
    val uri = DetectorConfig.getString(conf, "uri", DetectorCloud.DEF_URI)
    val cloud = Try(Cloud(uri)) match {
      case Success(cloud) => cloud
      case Failure(e) =>
        log.warn(s"${rx.getExtId()}: failed to create Cloud: ${e.getMessage}")
        error(desc, Some(s"Cloud: ${e.getMessage}"))
        return SentryRun.SENTRY_STOPPED
    }

    rx.get("cloud").asInstanceOf[Option[Cloud]].foreach(old => Try(old.close()))
    rx.set("cloud", cloud)

    rx.set("condition", DetectorConfig.getString(conf, "condition", DetectorCloud.DEF_CONDITION))
    rx.set("new_only", DetectorConfig.getBoolean(conf, "new_only", DetectorCloud.DEF_NEW_ONLY))
    rx.set("timeout", DetectorConfig.getLong(conf, "timeout", DetectorCloud.DEF_TIMEOUT))

    log.info(s"${rx.getExtId()}: ${cloud}: systems=${systems}")

    SentryRun.SENTRY_RUNNING
  }

  override def onStop(rx: SentryRun0, conf: DetectorConfig): Int = {
    rx.get("cloud").asInstanceOf[Option[Cloud]].foreach(c => Try(c.close()))
    super.onStop(rx, conf)
  }

  override def onCron(rx: SentryRun0, elapsed: Long): Seq[Event] = {
    val timeout = rx.get("timeout").asInstanceOf[Option[Long]].getOrElse(DetectorCloud.DEF_TIMEOUT)
    
    try {
      Await.result(checkCloud(rx), (timeout + 5000L).millis)
    } catch {
      case e: Exception =>
        log.warn(s"${rx.getExtId()}: cron failed: ${e.getMessage}")
        errorEvents(rx, "", "", e.getMessage)
    }
  }

  def checkCloud(rx: SentryRun0): Future[Seq[Event]] = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val cloud = rx.get("cloud").asInstanceOf[Option[Cloud]] match {
      case Some(cloud) => cloud
      case None =>
        log.warn(s"${rx.getExtId()}: cloud undefined")
        return Future.successful(Seq.empty)
    }

    val systems = rx.get("system").asInstanceOf[Option[Seq[String]]].getOrElse(Seq.empty)
    val timeout = rx.get("timeout").asInstanceOf[Option[Long]].getOrElse(DetectorCloud.DEF_TIMEOUT)

    val results: Future[Seq[(String, Try[Seq[CloudAlert]])]] = Future.traverse(systems) { system =>
      val f = Try(cloud.check(system)) match {
        case Success(f) => f
        case Failure(e) => Future.failed(e)
      }
      // timeout is enforced by onCron
      f.transform(t => Success(system -> t))
    }

    results.map { rr =>
      rr.flatMap {
        case (system, Success(alerts)) => 
          log.info(s"${rx.getExtId()}: ${cloud.provider}: ${system}: findings=${alerts.size}")
          processAlerts(rx, cloud, system, alerts)

        case (system, Failure(e)) =>
          log.warn(s"${rx.getExtId()}: ${cloud.provider}: ${system}: ${e.getMessage}")
          errorEvents(rx, cloud.provider, system, Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
      }
    }
  }

  def processAlerts(rx: SentryRun0, cloud: Cloud, system: String, alerts: Seq[CloudAlert]): Seq[Event] = {
    val newOnly = rx.get("new_only").asInstanceOf[Option[Boolean]].getOrElse(DetectorCloud.DEF_NEW_ONLY)
    val seenKey = s"seen_${system}"
    val seen = rx.get(seenKey).asInstanceOf[Option[Set[String]]].getOrElse(Set.empty)

    val selected = DetectorCloud.selectNew(alerts, seen, newOnly)
    
    // remember only current findings: resolved findings can alert again if they come back
    rx.set(seenKey, alerts.map(_.id).toSet)

    val condition = rx.get("condition").asInstanceOf[Option[String]].getOrElse(DetectorCloud.DEF_CONDITION)
    val threshold = new ThresholdDouble(0.0, condition)
    
    if (!threshold.set(selected.size)) {
      return Seq.empty
    }

    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorCloud.DEF_DESC)

    selected.map { a =>
      EventUtil.createEvent0(
        did,
        tx = None,
        monitoredAddr = None,
        conf = Some(rx.getConf()),
        meta = a.meta ++ Map(
          "provider" -> cloud.provider,
          "system" -> a.system,
          "id" -> a.id,
          "resource" -> a.resource,
          "status" -> a.status,
          "title" -> a.title,
          "count" -> selected.size.toString,
          "desc" -> desc
        ),
        detectorTs = if (a.ts > 0L) Some(a.ts.toString) else None,
        sev = Some(a.sev)
      )
    }
  }

  def errorEvents(rx: SentryRun0, provider: String, system: String, err: String): Seq[Event] = {
    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorCloud.DEF_DESC)
    error(rx,
      None, None, Some(rx.getConf()),
      desc,
      err,
      DetectorCloud.DEF_SEV_ERR,
      Map("provider" -> provider, "system" -> system, "title" -> "Check failed"),
      DetectorCloud.DEF_TRACK_ERR,
      DetectorCloud.DEF_TRACK_ERR_ALWAYS
    )
  }
}
