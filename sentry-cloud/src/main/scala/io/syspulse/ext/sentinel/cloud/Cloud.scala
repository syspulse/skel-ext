package io.syspulse.ext.sentinel.cloud

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import com.typesafe.scalalogging.Logger

/**
 * Finding (problem or event) reported by a Cloud. Detector converts it to Alert (Event).
 *
 * @param system   monitored system: Watch | IAM | Trail | Storage
 * @param id       unique id of the finding. The same problem must have the same id between checks
 *                 (it is used to alert only on new findings)
 * @param resource affected resource (alarm, user, bucket, ...)
 * @param status   status of the resource (ALARM, NOT_FOUND, MFA_DISABLED, ...)
 * @param sev      severity (see io.hacken.ext.core.Severity)
 * @param title    human readable summary
 * @param ts       time of the finding (epoch millis), 0 if unknown
 * @param meta     additional metadata for the Alert
 */
case class CloudAlert(
  system: String,
  id: String,
  resource: String,
  status: String,
  sev: Double,
  title: String,
  ts: Long = 0L,
  meta: Map[String, String] = Map.empty
)

/**
 * Generic Cloud services interface. 
 * Cloud specific functionality is implemented in dedicated classes: CloudAWS, CloudGCP, CloudAzure
 * 
 * All checks are asynchronous. Check returns problems only: empty result means that everything is healthy
 */
trait Cloud {
  protected val log = Logger(s"${this.getClass.getSimpleName}")

  def uri: CloudURI
  def provider: String

  /** Monitoring alarms (CloudWatch): VPC, EC2, EKS, ... health */
  def checkWatch(): Future[Seq[CloudAlert]]

  /** Identity and Access Management configuration alerts */
  def checkIAM(): Future[Seq[CloudAlert]]

  /** Audit events (CloudTrail) */
  def checkTrail(): Future[Seq[CloudAlert]]

  /** Storage (S3) availability */
  def checkStorage(): Future[Seq[CloudAlert]]

  def check(system: String): Future[Seq[CloudAlert]] = Cloud.system(system) match {
    case Some(Cloud.SYSTEM_WATCH) => checkWatch()
    case Some(Cloud.SYSTEM_IAM) => checkIAM()
    case Some(Cloud.SYSTEM_TRAIL) => checkTrail()
    case Some(Cloud.SYSTEM_STORAGE) => checkStorage()
    case _ => Future.failed(new IllegalArgumentException(s"unknown system: '${system}'"))
  }

  def close(): Unit = {}
}

object Cloud {
  val SYSTEM_WATCH = "Watch"        // CloudWatch
  val SYSTEM_IAM = "IAM"
  val SYSTEM_TRAIL = "Trail"        // CloudTrail
  val SYSTEM_STORAGE = "Storage"    // S3

  val SYSTEMS = Seq(SYSTEM_WATCH, SYSTEM_IAM, SYSTEM_TRAIL, SYSTEM_STORAGE)

  /** Normalize system name (case insensitive, with aliases) */
  def system(name: String): Option[String] = name.trim.toLowerCase match {
    case "watch" | "cloudwatch" | "monitoring" | "cw" => Some(SYSTEM_WATCH)
    case "iam" | "identity" => Some(SYSTEM_IAM)
    case "trail" | "cloudtrail" | "audit" => Some(SYSTEM_TRAIL)
    case "storage" | "s3" => Some(SYSTEM_STORAGE)
    case _ => None
  }

  /** Create Cloud from URI: aws://, gcp://, azure:// */
  def apply(uri: String): Cloud = {
    val cloud = CloudURI(uri).cloud
    cloud match {
      case "aws" => new CloudAWS(uri)
      case "gcp" => new CloudGCP(uri)
      case "azure" => new CloudAzure(uri)
      case _ => throw new IllegalArgumentException(s"unsupported cloud uri: '${cloud}' (expected: aws://, gcp://, azure://)")
    }
  }
}

/**
 * Base for mocked Clouds (GCP, Azure).
 *
 * Behavior is controlled by `mock` URI parameter:
 *  - `mock=alarm` (default): every system reports one sample finding
 *  - `mock=ok`: no findings
 *  - `mock=fail`: every check fails
 */
abstract class CloudMocked(uri0: String) extends Cloud {
  val uri = CloudURI(uri0)

  def mode: String = uri.param("mock", "alarm").toLowerCase

  /** sample findings for the system */
  protected def sample(system: String): Seq[CloudAlert]

  protected def mocked(system: String): Future[Seq[CloudAlert]] = {
    log.warn(s"${provider}: MOCKED: ${system}: mode=${mode}")
    mode match {
      case "fail" => Future.failed(new Exception(s"${provider}: mocked failure: ${system}"))
      case "ok" | "none" => Future.successful(Seq.empty)
      case _ => Future.successful(sample(system))
    }
  }

  def checkWatch(): Future[Seq[CloudAlert]] = mocked(Cloud.SYSTEM_WATCH)
  def checkIAM(): Future[Seq[CloudAlert]] = mocked(Cloud.SYSTEM_IAM)
  def checkTrail(): Future[Seq[CloudAlert]] = mocked(Cloud.SYSTEM_TRAIL)
  def checkStorage(): Future[Seq[CloudAlert]] = mocked(Cloud.SYSTEM_STORAGE)

  override def toString = s"${this.getClass.getSimpleName}(${uri})"
}
