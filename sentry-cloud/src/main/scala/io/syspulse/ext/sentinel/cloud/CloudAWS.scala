package io.syspulse.ext.sentinel.cloud

import java.nio.charset.StandardCharsets
import java.util.Date
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.{Try, Success, Failure}

import spray.json._

import com.amazonaws.AmazonServiceException
import com.amazonaws.auth.{STSSessionCredentialsProvider, AWSCredentialsProvider, AWSStaticCredentialsProvider, BasicAWSCredentials, BasicSessionCredentials, DefaultAWSCredentialsProviderChain, STSAssumeRoleSessionCredentialsProvider}
import com.amazonaws.auth.profile.{ProfileCredentialsProvider, ProfilesConfigFile}
import com.amazonaws.services.securitytoken.{AWSSecurityTokenService, AWSSecurityTokenServiceClientBuilder}
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration

import com.amazonaws.services.cloudwatch.{AmazonCloudWatch, AmazonCloudWatchClientBuilder}
import com.amazonaws.services.cloudwatch.model.{DescribeAlarmsRequest, MetricAlarm}

import com.amazonaws.services.identitymanagement.{AmazonIdentityManagement, AmazonIdentityManagementClientBuilder}
import com.amazonaws.services.identitymanagement.model.{GenerateCredentialReportRequest, GetCredentialReportRequest, CredentialReportNotReadyException, CredentialReportNotPresentException, CredentialReportExpiredException}

import com.amazonaws.services.cloudtrail.{AWSCloudTrail, AWSCloudTrailClientBuilder}
import com.amazonaws.services.cloudtrail.model.{LookupEventsRequest, LookupAttribute, LookupAttributeKey, Event => TrailEvent}

import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}
import com.amazonaws.services.s3.model.HeadBucketRequest

import io.hacken.ext.core.Severity
import io.syspulse.ext.sentinel.cloud.auth.{Auth, AuthURI}

object CloudAWS {
  /** AWS names of the systems */
  val SYS = Map(
    Cloud.SYSTEM_WATCH -> "CloudWatch",
    Cloud.SYSTEM_TRAIL -> "CloudTrail",
    Cloud.SYSTEM_IAM -> "IAM",
    Cloud.SYSTEM_STORAGE -> "S3"
  )

  // Credentials (see cloud.auth.Auth)
  val DEF_ROLE_SESSION = Auth.DEF_ROLE_SESSION
  val DEF_ROLE_DURATION = Auth.DEF_ROLE_DURATION

  val AUTH_URI = Auth.SOURCE_URI              // static keys in URI
  val AUTH_PROFILE = "profile"                // shared credentials file profile (refreshed)
  val AUTH_ENV = Auth.SOURCE_ENV              // static keys from environment
  val AUTH_DEFAULT = "default"                // AWS default chain (refreshed): web identity, container, EC2 instance profile ...

  /** Base source of credentials (from URI only): URI keys > profile > environment keys > default chain */
  def authBase(uri: CloudURI): String = Auth.resolve(uri, None).base match {
    case s: Auth.Static => s.source
    case _: Auth.Profile => AUTH_PROFILE
    case _ => AUTH_DEFAULT
  }

  /** Session token which came together with static keys (same source). Such credentials EXPIRE and cannot be refreshed */
  def staticToken(uri: CloudURI): Option[String] = Auth.resolve(uri, None).base match {
    case s: Auth.Static => s.token
    case _ => None
  }

  /** 
   * Long-lived keys (URI or environment) without SESSION_TOKEN: session token is requested from STS (GetSessionToken) 
   * and refreshed automatically before it expires. Disable with `sts=false`. 
   * Not needed with `role_arn`: keys are used to assume the role
   */
  def requestsToken(uri: CloudURI): Boolean = Auth.resolve(uri, None).base match {
    case s: Auth.Static => s.requestToken
    case _ => false
  }

  /** Description of authentication (for logs and tests). No secrets */
  def authName(uri: CloudURI): String = Auth.resolve(uri, None).name

  /** Static SESSION_TOKEN is used: credentials expire and cannot be refreshed */
  def isStatic(uri: CloudURI): Boolean = Auth.resolve(uri, None).isStatic

  // Watch (CloudWatch)
  val DEF_STATES = "ALARM"                    // alarm states to report: ALARM,INSUFFICIENT_DATA
  val DEF_ALARM_PREFIX = ""                   // alarm name prefix filter
  val DEF_NAMESPACE = ""                      // metric namespaces filter (comma-separated): AWS/EC2,AWS/EKS,AWS/VPC,AWS/S3

  // IAM
  val DEF_MAX_KEY_AGE = 90                    // days

  // Trail (CloudTrail)
  val DEF_TRAIL_SOURCE = ""                   // event sources (comma-separated): iam.amazonaws.com, s3.amazonaws.com. Empty = all (write events only)
  val DEF_TRAIL_EVENTS = ""                   // event names filter (comma-separated). Empty = all
  val DEF_TRAIL_EVENT_SEV = Severity.INFO     // severity of read-only events (generated if `track_event` is enabled in Detector config) (write events: HIGH for security sensitive, MEDIUM for others)
  val DEF_WINDOW = 60                         // minutes
  val DEF_MAX_EVENTS = 200

  // Storage
  val DEF_BUCKETS = ""                        // explicit list of buckets (comma-separated). Empty = all buckets
  val DEF_BUCKET_PREFIX = ""
  val DEF_LATENCY = 0L                        // ms, 0 = do not check

  val DEF_REPORT_RETRIES = 10
  val DEF_REPORT_DELAY = 1000L

  val ROOT_ACCOUNT = "<root_account>"

  // global services events are recorded by CloudTrail in us-east-1
  val GLOBAL_SOURCES = Set("iam.amazonaws.com", "sts.amazonaws.com", "signin.amazonaws.com", "organizations.amazonaws.com", "cloudfront.amazonaws.com", "route53.amazonaws.com")
  val GLOBAL_REGION = "us-east-1"

  // ----- Console links (ref) -------------------------------------------------------------------------------------
  def consoleHost(region: String): String = if (region.isEmpty) "https://console.aws.amazon.com" else s"https://${region}.console.aws.amazon.com"

  private def enc(s: String): String = java.net.URLEncoder.encode(s, "UTF-8").replace("+", "%20")

  def refTrail(eventId: String, region: String): String =
    if (eventId.isEmpty) "" else s"${consoleHost(region)}/cloudtrail/home?region=${if (region.isEmpty) GLOBAL_REGION else region}#/events/${enc(eventId)}"

  def refAlarm(name: String, region: String): String =
    if (name.isEmpty) "" else s"${consoleHost(region)}/cloudwatch/home?region=${if (region.isEmpty) GLOBAL_REGION else region}#alarmsV2:alarm/${enc(name)}"

  def refIAM(user: String): String =
    if (user == ROOT_ACCOUNT) "https://console.aws.amazon.com/iam/home#/security_credentials"
    else s"https://console.aws.amazon.com/iam/home#/users/details/${enc(user)}?section=security_credentials"

  def refS3(bucket: String): String = s"https://s3.console.aws.amazon.com/s3/buckets/${enc(bucket)}"

  // ----- Watch (CloudWatch) --------------------------------------------------------------------------------------
  def severityAlarm(state: String): Double = state match {
    case "ALARM" => Severity.HIGH
    case "INSUFFICIENT_DATA" => Severity.LOW
    case _ => Severity.INFO
  }

  /** @param region region of the Cloud (used for the link if ARN of the alarm does not have it) */
  def fromAlarm(a: MetricAlarm, region: String = ""): CloudAlert = {
    val name = Option(a.getAlarmName).getOrElse("")
    val state = Option(a.getStateValue).getOrElse("")
    val ts = Option(a.getStateUpdatedTimestamp).map(_.getTime).getOrElse(0L)
    val arn = Option(a.getAlarmArn).getOrElse(name)
    val dims = Option(a.getDimensions).map(_.asScala.map(d => s"${d.getName}=${d.getValue}").mkString(",")).getOrElse("")

    CloudAlert(
      Cloud.SYSTEM_WATCH,
      // state change generates new id
      id = s"${arn}:${state}:${ts}",
      resource = name,
      status = state,
      sev = severityAlarm(state),
      title = s"${name}: ${state}",
      ts = ts,
      meta = Map(
        "provider" -> "aws",
        "alarm" -> name,
        "arn" -> arn,
        "state" -> state,
        "reason" -> Option(a.getStateReason).getOrElse(""),
        "namespace" -> Option(a.getNamespace).getOrElse(""),
        "metric" -> Option(a.getMetricName).getOrElse(""),
        "dimensions" -> dims,
        "description" -> Option(a.getAlarmDescription).getOrElse("")
      ),
      // ARN: arn:aws:cloudwatch:<region>:<account>:alarm:<name>
      ref = refAlarm(name, arn.split(":", 6).lift(3).filter(_.nonEmpty).getOrElse(region)),
      uid = arn
    )
  }

  // ----- IAM -----------------------------------------------------------------------------------------------------
  private def parseTs(s: String): Option[Long] =
    Try(java.time.OffsetDateTime.parse(s).toInstant.toEpochMilli).toOption

  /**
   * Analyze IAM Credential Report (CSV) 
   * https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html
   */
  def parseCredentialReport(csv: String, now: Long = System.currentTimeMillis(), maxKeyAgeDays: Int = DEF_MAX_KEY_AGE): Seq[CloudAlert] = {
    val lines = csv.split("\r?\n").toSeq.filter(_.trim.nonEmpty)
    if (lines.size < 2) return Seq.empty

    val header = lines.head.split(",", -1).map(_.trim).toSeq

    lines.tail.flatMap { line =>
      val row = header.zip(line.split(",", -1).map(_.trim).toSeq).toMap
      val user = row.getOrElse("user", "")
      val arn = row.getOrElse("arn", user)
      val isRoot = user == ROOT_ACCOUNT
      def isTrue(k: String) = row.get(k).exists(_.equalsIgnoreCase("true"))

      def alert(check: String, status: String, sev: Double, title: String, meta: Map[String, String] = Map.empty) =
        CloudAlert(Cloud.SYSTEM_IAM, s"iam:${user}:${check}", user, status, sev, title, now,
          Map("provider" -> "aws", "user" -> user, "arn" -> arn, "check" -> check) ++ meta,
          ref = refIAM(user), uid = arn)

      val rootKeys =
        if (isRoot && (isTrue("access_key_1_active") || isTrue("access_key_2_active")))
          Seq(alert("root-access-key", "ROOT_ACCESS_KEY", Severity.CRITICAL, "Root account has active access keys"))
        else Seq.empty

      val mfa =
        if (!isTrue("mfa_active")) {
          if (isRoot)
            Seq(alert("root-mfa", "MFA_DISABLED", Severity.HIGH, "Root account has no MFA"))
          else if (isTrue("password_enabled"))
            Seq(alert("mfa", "MFA_DISABLED", Severity.MEDIUM, s"User '${user}' has console access without MFA"))
          else Seq.empty
        } else Seq.empty

      val keys = if (isRoot) Seq.empty else Seq(1, 2).flatMap { n =>
        if (isTrue(s"access_key_${n}_active")) {
          row.get(s"access_key_${n}_last_rotated").flatMap(parseTs).flatMap { rotated =>
            val age = (now - rotated) / (24L * 3600L * 1000L)
            if (age > maxKeyAgeDays)
              Some(alert(s"key${n}-age", "KEY_OLD", Severity.MEDIUM, s"User '${user}' access key ${n} is ${age} days old (max ${maxKeyAgeDays})",
                Map("age_days" -> age.toString, "key" -> n.toString)))
            else None
          }
        } else None
      }

      rootKeys ++ mfa ++ keys
    }
  }

  // ----- Trail (CloudTrail) --------------------------------------------------------------------------------------
  val READ_ONLY_PREFIX = Seq("Get", "List", "Generate", "Simulate", "Describe", "Lookup", "Head")

  // security sensitive management events
  val HIGH_RISK = Set(
    // IAM
    "CreateUser", "DeleteUser", "CreateRole", "DeleteRole",
    "CreateAccessKey", "CreateLoginProfile", "UpdateLoginProfile",
    "AttachUserPolicy", "AttachRolePolicy", "AttachGroupPolicy", 
    "PutUserPolicy", "PutRolePolicy", "PutGroupPolicy",
    "CreatePolicyVersion", "SetDefaultPolicyVersion", "UpdateAssumeRolePolicy",
    "AddUserToGroup", "DeactivateMFADevice", "DeleteVirtualMFADevice", 
    "DeleteAccountPasswordPolicy", "UpdateAccountPasswordPolicy",
    // CloudTrail
    "StopLogging", "DeleteTrail", "UpdateTrail", "PutEventSelectors",
    // network, storage, keys
    "AuthorizeSecurityGroupIngress", "DeleteFlowLogs", 
    "PutBucketPolicy", "DeleteBucketPolicy", "PutBucketAcl", "DeleteBucketPublicAccessBlock", "PutBucketPublicAccessBlock",
    "DisableKey", "ScheduleKeyDeletion"
  )

  /** 
   * Severity of the CloudTrail event. None for read-only events (ignored).
   * @param readOnly `ReadOnly` attribute of the event ("true"/"false"), if CloudTrail returned it
   * @param trackEvent generate Alerts for read-only events with DEF_TRAIL_EVENT_SEV (INFO)
   */
  def classifyTrailEvent(name: String, readOnly: Option[String] = None, trackEvent: Boolean = false): Option[Double] = {
    val isRead = readOnly match {
      case Some(r) => r.equalsIgnoreCase("true")
      case None => READ_ONLY_PREFIX.exists(p => name.startsWith(p))
    }
    if (isRead) { if (trackEvent) Some(DEF_TRAIL_EVENT_SEV) else None }
    else if (HIGH_RISK.contains(name)) Some(Severity.HIGH)
    else Some(Severity.MEDIUM)
  }

  /** string field of the raw CloudTrail event JSON */
  def trailField(raw: String, field: String): Option[String] = {
    if (raw == null || raw.isEmpty) return None
    Try(raw.parseJson.asJsObject.fields.get(field)).toOption.flatten.collect { case JsString(s) => s }
  }

  /** error code from the raw CloudTrail event JSON (AccessDenied, ...) */
  def trailErrorCode(raw: String): String = trailField(raw, "errorCode").getOrElse("")

  /** @param region region where the event was looked up (used for the link if the event does not have `awsRegion`) */
  def fromTrailEvent(e: TrailEvent, trackEvent: Boolean = false, region: String = ""): Option[CloudAlert] = {
    val name = Option(e.getEventName).getOrElse("")
    classifyTrailEvent(name, Option(e.getReadOnly), trackEvent).map { sev =>
      val user = Option(e.getUsername).getOrElse("")
      val src = Option(e.getEventSource).getOrElse("")
      val ts = Option(e.getEventTime).map(_.getTime).getOrElse(0L)
      val res = Option(e.getResources).map(_.asScala.map(_.getResourceName).filter(_ != null).toSeq).getOrElse(Seq.empty)
      CloudAlert(
        Cloud.SYSTEM_TRAIL,
        id = s"trail:${Option(e.getEventId).getOrElse(s"${name}:${user}:${ts}")}",
        resource = res.headOption.getOrElse(user),
        status = name,
        sev = sev,
        title = s"${src.stripSuffix(".amazonaws.com")} ${name} by '${user}'" + (if (res.nonEmpty) s" on ${res.mkString(",")}" else ""),
        ts = ts,
        meta = Map(
          "provider" -> "aws",
          "event" -> name,
          "event_id" -> Option(e.getEventId).getOrElse(""),
          "user" -> user,
          "resources" -> res.mkString(","),
          "source" -> src,
          "error_code" -> trailErrorCode(e.getCloudTrailEvent)
        ),
        ref = refTrail(Option(e.getEventId).getOrElse(""), trailField(e.getCloudTrailEvent, "awsRegion").filter(_.nonEmpty).getOrElse(region)),
        uid = Option(e.getEventId).getOrElse("")
      )
    }
  }

  // ----- Storage -------------------------------------------------------------------------------------------------
  /** Status and severity of the failed S3 request */
  def statusS3(e: Throwable): (String, Double) = e match {
    case s: AmazonServiceException if s.getStatusCode == 403 => ("ACCESS_DENIED", Severity.MEDIUM)
    case s: AmazonServiceException if s.getStatusCode == 404 => ("NOT_FOUND", Severity.HIGH)
    case s: AmazonServiceException if s.getStatusCode >= 500 => ("UNAVAILABLE", Severity.CRITICAL)
    case _: AmazonServiceException => ("ERROR", Severity.HIGH)
    case _ => ("UNREACHABLE", Severity.CRITICAL)
  }

  def alertS3(bucket: String, status: String, sev: Double, err: String, latency: Long): CloudAlert =
    CloudAlert(
      Cloud.SYSTEM_STORAGE,
      id = s"s3:${bucket}:${status}",
      resource = bucket,
      status = status,
      sev = sev,
      title = s"S3 bucket '${bucket}': ${status}",
      ts = System.currentTimeMillis(),
      meta = Map("provider" -> "aws", "bucket" -> bucket, "latency" -> latency.toString, "error" -> err),
      ref = refS3(bucket),
      uid = s"arn:aws:s3:::${bucket}"
    )
}

/**
 * AWS Cloud: CloudWatch alarms (Watch), IAM credential report (IAM), CloudTrail events (Trail), S3 availability (Storage)
 * 
 * aws://[ACCESS_KEY:ACCESS_SECRET[:SESSION_TOKEN]@][host]?region=eu-west-1&param=value
 * 
 * Parameters:
 *   Watch:   states=ALARM,INSUFFICIENT_DATA & alarm_prefix=prod- & namespace=AWS/EC2,AWS/EKS
 *   IAM:     max_key_age=90
 *   Trail:   source=iam.amazonaws.com,s3.amazonaws.com & events=CreateUser,DeleteUser & window=60 & trail_region=us-east-1
 *   Storage: buckets=a,b & bucket_prefix=prod- & latency=2000
 * 
 * AWS SDK is blocking, all calls are executed asynchronously.
 * If credentials are not found in URI or environment, AWS default credentials chain is used
 * 
 * Custom endpoint (host in URI) is used for all services: LocalStack, moto: aws://localhost:4566?region=us-east-1&ssl=false
 */
class CloudAWS(uri0: String, auth0: Option[String] = None) extends Cloud {
  import CloudAWS._

  val uri = CloudURI(uri0)
  def provider: String = "aws"

  /** Authentication (`auth`) has a preference over credentials in `uri`. Invalid `auth` fails immediately */
  val authUri: Option[AuthURI] = auth0.map(_.trim).filter(_.nonEmpty).map(a => AuthURI(a))
  val authSpec: Auth = Auth.resolve(uri, authUri)

  override def sys(system: String): String = SYS.getOrElse(system, system)

  implicit private val ec: ExecutionContext = ExecutionContext.global

  override def toString = s"CloudAWS(${uri})"

  /** How credentials are obtained, e.g. `profile`, `role(default)`. Never contains secrets */
  def auth: String = authSpec.name

  private val stsClients = new java.util.concurrent.CopyOnWriteArrayList[AWSSecurityTokenService]()

  // STS client must use the same endpoint (mock) or region as the other clients
  private def stsClient(creds: AWSCredentialsProvider): AWSSecurityTokenService = {
    val sts = AWSSecurityTokenServiceClientBuilder.standard().withCredentials(creds)
    val c = (endpointConfig(uri.region) match {
      case Some(ep) => sts.withEndpointConfiguration(ep)
      case None => uri.region.map(r => sts.withRegion(r)).getOrElse(sts)
    }).build()
    stsClients.add(c)
    c
  }

  private val sessions = new java.util.concurrent.CopyOnWriteArrayList[STSSessionCredentialsProvider]()
  private val stsList = new java.util.concurrent.CopyOnWriteArrayList[STSAssumeRoleSessionCredentialsProvider]()

  // Everything except static keys is refreshed by AWS SDK
  private def credentialsFor(a: Auth): AWSCredentialsProvider = a match {
    case Auth.Static(k, s, Some(t), _, _, _) =>
      log.warn(s"${uri}: static SESSION_TOKEN is used: credentials will EXPIRE and are NOT refreshed. Do not specify SESSION_TOKEN (it will be requested and refreshed), use `profile`, `role` or default credentials chain")
      new AWSStaticCredentialsProvider(new BasicSessionCredentials(k, s, t))

    case Auth.Static(k, s, None, _, request, _) => 
      val keys = new AWSStaticCredentialsProvider(new BasicAWSCredentials(k, s))
      if (request) {
        // no SESSION_TOKEN: request it with long-lived keys, SDK refreshes it before expiration
        val p = new STSSessionCredentialsProvider(stsClient(keys))
        sessions.add(p)
        p
      } else keys

    // shared credentials file: re-read periodically (rotated keys, credential_process, role_arn + source_profile)
    case Auth.Profile(name, file) =>
      file match {
        case None => new ProfileCredentialsProvider(name)
        case Some(f) => new ProfileCredentialsProvider(new ProfilesConfigFile(f), name)
      }

    // env, system properties, profile, web identity (IRSA), container (ECS), instance profile (EC2)
    case _: Auth.Default => DefaultAWSCredentialsProviderChain.getInstance()

    case r: Auth.Role =>
      val b = new STSAssumeRoleSessionCredentialsProvider.Builder(r.arn, r.session).withRoleSessionDurationSeconds(r.duration)
      r.externalId.foreach(id => b.withExternalId(id))

      // STS client (it carries credentials of the underlying source) must use the same endpoint (mock) or region as the other clients
      b.withStsClient(stsClient(credentialsFor(r.underlying)))

      val p = b.build()
      stsList.add(p)
      p
  }

  // ONE provider shared by all clients: it caches and refreshes credentials 
  protected lazy val credentials: AWSCredentialsProvider = {
    val p = credentialsFor(authSpec)
    val src = authSpec.base match {
      case s: Auth.Static if s.detail.nonEmpty => s" (${s.source}: ${s.detail})"
      case _ => ""
    }
    log.info(s"${uri}: credentials: ${auth}${src}" + authUri.map(a => s", auth=${a}").getOrElse(""))
    p
  }

  private def endpointConfig(region: Option[String]): Option[EndpointConfiguration] =
    uri.endpoint.map(ep => new EndpointConfiguration(ep, region.getOrElse("us-east-1")))

  private class Lazy[T](build: => T, stop: T => Unit) {
    private var v: Option[T] = None
    def get: T = synchronized { if (v.isEmpty) v = Some(build); v.get }
    def close(): Unit = synchronized { v.foreach(c => Try(stop(c))); v = None }
  }

  private val cloudwatch = new Lazy[AmazonCloudWatch]({
    val b = AmazonCloudWatchClientBuilder.standard().withCredentials(credentials)
    (endpointConfig(uri.region) match {
      case Some(ep) => b.withEndpointConfiguration(ep)
      case None => uri.region.map(r => b.withRegion(r)).getOrElse(b)
    }).build()
  }, (c: AmazonCloudWatch) => c.shutdown())

  // IAM is global
  private val iam = new Lazy[AmazonIdentityManagement]({
    val b = AmazonIdentityManagementClientBuilder.standard().withCredentials(credentials)
    (endpointConfig(uri.region) match {
      case Some(ep) => b.withEndpointConfiguration(ep)
      case None => b.withRegion(uri.region.getOrElse(GLOBAL_REGION))
    }).build()
  }, (c: AmazonIdentityManagement) => c.shutdown())

  // CloudTrail clients per region
  private val trails = new ConcurrentHashMap[String, AWSCloudTrail]()
  private def trail(region: Option[String]): AWSCloudTrail = {
    trails.computeIfAbsent(region.getOrElse(""), _ => {
      val b = AWSCloudTrailClientBuilder.standard().withCredentials(credentials)
      (endpointConfig(region) match {
        case Some(ep) => b.withEndpointConfiguration(ep)
        case None => region.map(r => b.withRegion(r)).getOrElse(b)
      }).build()
    })
  }

  private val s3 = new Lazy[AmazonS3]({
    val b = AmazonS3ClientBuilder.standard().withCredentials(credentials)
    (endpointConfig(uri.region) match {
      // custom endpoint (LocalStack, MinIO, moto): no cross-region redirects
      case Some(ep) => b.withEndpointConfiguration(ep).withPathStyleAccessEnabled(true)
      // buckets from other regions are accessible
      case None => uri.region.map(r => b.withRegion(r)).getOrElse(b).withForceGlobalBucketAccessEnabled(true)
    }).build()
  }, (c: AmazonS3) => c.shutdown())

  override def close(): Unit = {
    cloudwatch.close()
    iam.close()
    trails.values().asScala.foreach(c => Try(c.shutdown()))
    trails.clear()
    s3.close()
    stsList.asScala.foreach(p => Try(p.close()))
    stsList.clear()
    sessions.asScala.foreach(p => Try(p.close()))
    sessions.clear()
    stsClients.asScala.foreach(c => Try(c.shutdown()))
    stsClients.clear()
  }

  // ----- Watch (CloudWatch) --------------------------------------------------------------------------------------
  def checkWatch(): Future[Seq[CloudAlert]] = Future {
    blocking {
      val states = {
        val s = uri.paramList("states")
        if (s.isEmpty) DEF_STATES.split(",").toSeq else s.map(_.toUpperCase)
      }
      val prefix = uri.param("alarm_prefix", DEF_ALARM_PREFIX)
      val namespaces = uri.paramList("namespace")

      val alarms = states.flatMap { state =>
        val all = scala.collection.mutable.Buffer[MetricAlarm]()
        var token: String = null
        do {
          val req = new DescribeAlarmsRequest().withStateValue(state).withMaxRecords(100)
          if (token != null) req.setNextToken(token)
          if (prefix.nonEmpty) req.setAlarmNamePrefix(prefix)

          val rsp = cloudwatch.get.describeAlarms(req)
          all ++= rsp.getMetricAlarms.asScala
          token = rsp.getNextToken
        } while (token != null)
        all.toSeq
      }

      log.info(s"${uri}: Watch: alarms=${alarms.size}")

      alarms
        // state filter is also applied here: not all AWS compatible services support it
        .filter(a => states.contains(a.getStateValue))
        .filter(a => namespaces.isEmpty || namespaces.contains(a.getNamespace))
        .map(a => fromAlarm(a, uri.region.getOrElse("")))
    }
  }

  // ----- IAM -----------------------------------------------------------------------------------------------------
  def checkIAM(): Future[Seq[CloudAlert]] = Future {
    blocking {
      val maxKeyAge = Try(uri.param("max_key_age", DEF_MAX_KEY_AGE.toString).toInt).getOrElse(DEF_MAX_KEY_AGE)

      iam.get.generateCredentialReport(new GenerateCredentialReportRequest())

      def report(retries: Int): String = {
        try {
          val bb = iam.get.getCredentialReport(new GetCredentialReportRequest()).getContent
          val bytes = new Array[Byte](bb.remaining())
          bb.get(bytes)
          new String(bytes, StandardCharsets.UTF_8)
        } catch {
          case _: CredentialReportNotReadyException | _: CredentialReportNotPresentException | _: CredentialReportExpiredException if retries > 0 =>
            Thread.sleep(DEF_REPORT_DELAY)
            report(retries - 1)
        }
      }

      val alerts = parseCredentialReport(report(DEF_REPORT_RETRIES), System.currentTimeMillis(), maxKeyAge)
      log.info(s"${uri}: IAM: credential report: alerts=${alerts.size}")
      alerts
    }
  }

  // ----- Trail (CloudTrail) --------------------------------------------------------------------------------------
  def checkTrail(trackEvent: Boolean): Future[Seq[CloudAlert]] = Future {
    blocking {
      val window = Try(uri.param("window", DEF_WINDOW.toString).toLong).getOrElse(DEF_WINDOW.toLong)
      val names = uri.paramList("events").toSet
      val sources = uri.paramList("source")
      val now = System.currentTimeMillis()


      // CloudTrail allows only one lookup attribute: EventSource (per source) or ReadOnly=false for everything.
      // Read-only events are requested without attributes (everything)
      val lookups: Seq[(Option[String], Option[LookupAttribute])] = 
        if (sources.nonEmpty) 
          sources.map(s => (Some(s), Some(new LookupAttribute().withAttributeKey(LookupAttributeKey.EventSource).withAttributeValue(s))))
        else if (trackEvent)
          Seq((None, None))
        else 
          Seq((None, Some(new LookupAttribute().withAttributeKey(LookupAttributeKey.ReadOnly).withAttributeValue("false"))))

      val events = lookups.flatMap { case (source, attr) =>
        val region = uri.params.get("trail_region").filter(_.nonEmpty)
          .orElse(if (source.exists(GLOBAL_SOURCES.contains)) Some(GLOBAL_REGION) else uri.region)

        val all = scala.collection.mutable.Buffer[TrailEvent]()
        var token: String = null
        do {
          val req = new LookupEventsRequest()
            .withStartTime(new Date(now - window * 60L * 1000L))
            .withEndTime(new Date(now))
            .withMaxResults(50)
          attr.foreach(a => req.withLookupAttributes(a))
          if (token != null) req.setNextToken(token)

          val rsp = trail(region).lookupEvents(req)
          all ++= rsp.getEvents.asScala
          token = rsp.getNextToken
        } while (token != null && all.size < DEF_MAX_EVENTS)
        
        all.toSeq.map(e => (e, region.getOrElse("")))
      }

      val alerts = events
        .flatMap { case (e, region) => fromTrailEvent(e, trackEvent, region) }
        .filter(a => names.isEmpty || names.contains(a.status))
        .distinctBy(_.id)

      log.info(s"${uri}: Trail: events=${events.size}, alerts=${alerts.size}")
      alerts
    }
  }

  // ----- Storage -------------------------------------------------------------------------------------------------
  def checkStorage(): Future[Seq[CloudAlert]] = {
    val latencyMax = Try(uri.param("latency", DEF_LATENCY.toString).toLong).getOrElse(DEF_LATENCY)
    val prefix = uri.param("bucket_prefix", DEF_BUCKET_PREFIX)

    val buckets: Future[Seq[String]] = {
      val explicit = uri.paramList("buckets")
      if (explicit.nonEmpty) Future.successful(explicit)
      else Future {
        blocking {
          s3.get.listBuckets().asScala.map(_.getName).toSeq
        }
      }
    }

    buckets.flatMap { bb =>
      val selected = bb.filter(_.startsWith(prefix))
      log.info(s"${uri}: Storage: buckets=${selected.size}")

      Future.traverse(selected) { bucket =>
        Future {
          blocking {
            val t0 = System.nanoTime()
            try {
              s3.get.headBucket(new HeadBucketRequest(bucket))
              val latency = (System.nanoTime() - t0) / 1000000L
              if (latencyMax > 0 && latency > latencyMax)
                Seq(alertS3(bucket, "SLOW", Severity.LOW, s"latency ${latency} ms > ${latencyMax} ms", latency))
              else
                Seq.empty[CloudAlert]
            } catch {
              case e: Exception =>
                val latency = (System.nanoTime() - t0) / 1000000L
                val (status, sev) = statusS3(e)
                Seq(alertS3(bucket, status, sev, Option(e.getMessage).getOrElse(e.getClass.getSimpleName), latency))
            }
          }
        }
      }.map(_.flatten)
    }
  }
}
