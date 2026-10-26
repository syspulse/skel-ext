package io.syspulse.ext.sentinel.cloud

import java.util.Date

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.amazonaws.AmazonServiceException
import com.amazonaws.SdkClientException
import com.amazonaws.services.cloudwatch.model.{Dimension, MetricAlarm}
import com.amazonaws.services.cloudtrail.model.{Event => TrailEvent, Resource}

import io.hacken.ext.core.Severity

class CloudAWSSpec extends AnyFlatSpec with Matchers {

  // ---- CloudWatch ----
  "CloudAWS.fromAlarm" should "map ALARM" in {
    val ts = 1700000000000L
    val a = new MetricAlarm()
      .withAlarmName("ec2-cpu-high")
      .withAlarmArn("arn:aws:cloudwatch:eu-west-1:123:alarm:ec2-cpu-high")
      .withStateValue("ALARM")
      .withStateReason("Threshold Crossed")
      .withStateUpdatedTimestamp(new Date(ts))
      .withNamespace("AWS/EC2")
      .withMetricName("CPUUtilization")
      .withDimensions(new Dimension().withName("InstanceId").withValue("i-123"))

    val c = CloudAWS.fromAlarm(a)
    c.system shouldBe Cloud.SYSTEM_WATCH
    c.resource shouldBe "ec2-cpu-high"
    c.status shouldBe "ALARM"
    c.sev shouldBe Severity.HIGH
    c.ts shouldBe ts
    c.id shouldBe s"arn:aws:cloudwatch:eu-west-1:123:alarm:ec2-cpu-high:ALARM:${ts}"
    c.title shouldBe "ec2-cpu-high: ALARM"
    c.meta("namespace") shouldBe "AWS/EC2"
    c.meta("metric") shouldBe "CPUUtilization"
    c.meta("dimensions") shouldBe "InstanceId=i-123"
    c.meta("reason") shouldBe "Threshold Crossed"
  }

  it should "generate new id on state change" in {
    def alarm(state: String, ts: Long) = new MetricAlarm().withAlarmName("a1").withStateValue(state).withStateUpdatedTimestamp(new Date(ts))
    CloudAWS.fromAlarm(alarm("ALARM", 1000L)).id should not be CloudAWS.fromAlarm(alarm("ALARM", 2000L)).id
    CloudAWS.fromAlarm(alarm("ALARM", 1000L)).id shouldBe CloudAWS.fromAlarm(alarm("ALARM", 1000L)).id
  }

  it should "map severity by state" in {
    CloudAWS.severityAlarm("ALARM") shouldBe Severity.HIGH
    CloudAWS.severityAlarm("INSUFFICIENT_DATA") shouldBe Severity.LOW
    CloudAWS.severityAlarm("OK") shouldBe Severity.INFO
  }

  it should "handle alarm without optional fields" in {
    val c = CloudAWS.fromAlarm(new MetricAlarm().withAlarmName("a1").withStateValue("ALARM"))
    c.ts shouldBe 0L
    c.meta("dimensions") shouldBe ""
  }

  // ---- IAM credential report ----
  val HEADER = "user,arn,user_creation_time,password_enabled,password_last_used,password_last_changed,password_next_rotation,mfa_active," +
    "access_key_1_active,access_key_1_last_rotated,access_key_1_last_used_date,access_key_1_last_used_region,access_key_1_last_used_service," +
    "access_key_2_active,access_key_2_last_rotated,access_key_2_last_used_date,access_key_2_last_used_region,access_key_2_last_used_service," +
    "cert_1_active,cert_1_last_rotated,cert_2_active,cert_2_last_rotated"

  val NOW = java.time.OffsetDateTime.parse("2025-06-01T00:00:00+00:00").toInstant.toEpochMilli

  def report(rows: String*) = (HEADER +: rows).mkString("\n")

  "CloudAWS.parseCredentialReport" should "return nothing for healthy accounts" in {
    val csv = report(
      "<root_account>,arn:aws:iam::123:root,2020-01-01T00:00:00+00:00,not_supported,2025-05-01T00:00:00+00:00,not_supported,not_supported,true,false,N/A,N/A,N/A,N/A,false,N/A,N/A,N/A,N/A,false,N/A,false,N/A",
      "alice,arn:aws:iam::123:user/alice,2024-01-01T00:00:00+00:00,true,2025-05-30T00:00:00+00:00,2025-01-01T00:00:00+00:00,N/A,true,true,2025-05-01T00:00:00+00:00,2025-05-30T00:00:00+00:00,eu-west-1,s3,false,N/A,N/A,N/A,N/A,false,N/A,false,N/A"
    )
    CloudAWS.parseCredentialReport(csv, NOW, 90) shouldBe empty
  }

  it should "detect root access keys and root without MFA" in {
    val csv = report(
      "<root_account>,arn:aws:iam::123:root,2020-01-01T00:00:00+00:00,not_supported,2025-05-01T00:00:00+00:00,not_supported,not_supported,false,true,2020-01-01T00:00:00+00:00,N/A,N/A,N/A,false,N/A,N/A,N/A,N/A,false,N/A,false,N/A"
    )
    val aa = CloudAWS.parseCredentialReport(csv, NOW, 90)
    aa.map(_.id) should contain allOf ("iam:<root_account>:root-access-key", "iam:<root_account>:root-mfa")
    aa.find(_.id.endsWith("root-access-key")).get.sev shouldBe Severity.CRITICAL
    aa.find(_.id.endsWith("root-mfa")).get.sev shouldBe Severity.HIGH
    // root keys age is not checked
    aa.exists(_.id.contains("key1-age")) shouldBe false
  }

  it should "detect console user without MFA" in {
    val csv = report(
      "bob,arn:aws:iam::123:user/bob,2024-01-01T00:00:00+00:00,true,2025-05-30T00:00:00+00:00,2025-01-01T00:00:00+00:00,N/A,false,false,N/A,N/A,N/A,N/A,false,N/A,N/A,N/A,N/A,false,N/A,false,N/A"
    )
    val aa = CloudAWS.parseCredentialReport(csv, NOW, 90)
    aa should have size 1
    aa.head.id shouldBe "iam:bob:mfa"
    aa.head.resource shouldBe "bob"
    aa.head.sev shouldBe Severity.MEDIUM
  }

  it should "ignore programmatic user without console access and MFA" in {
    val csv = report(
      "svc,arn:aws:iam::123:user/svc,2024-01-01T00:00:00+00:00,false,N/A,N/A,N/A,false,true,2025-05-01T00:00:00+00:00,N/A,N/A,N/A,false,N/A,N/A,N/A,N/A,false,N/A,false,N/A"
    )
    CloudAWS.parseCredentialReport(csv, NOW, 90) shouldBe empty
  }

  it should "detect old access keys" in {
    val csv = report(
      "svc,arn:aws:iam::123:user/svc,2023-01-01T00:00:00+00:00,false,N/A,N/A,N/A,false,true,2024-01-01T00:00:00+00:00,N/A,N/A,N/A,true,2025-05-20T00:00:00+00:00,N/A,N/A,N/A,false,N/A,false,N/A"
    )
    val aa = CloudAWS.parseCredentialReport(csv, NOW, 90)
    aa should have size 1
    aa.head.id shouldBe "iam:svc:key1-age"
    aa.head.status shouldBe "KEY_OLD"
    aa.head.meta("key") shouldBe "1"
    aa.head.meta("age_days").toLong should be > 90L

    // longer max age 
    CloudAWS.parseCredentialReport(csv, NOW, 1000) shouldBe empty
  }

  it should "handle empty report" in {
    CloudAWS.parseCredentialReport("", NOW) shouldBe empty
    CloudAWS.parseCredentialReport(HEADER, NOW) shouldBe empty
  }

  // ---- IAM events ----
  "CloudAWS.classifyTrailEvent" should "ignore read-only events" in {
    CloudAWS.classifyTrailEvent("GetUser") shouldBe None
    CloudAWS.classifyTrailEvent("ListRoles") shouldBe None
    CloudAWS.classifyTrailEvent("GenerateCredentialReport") shouldBe None
    // ReadOnly attribute has priority
    CloudAWS.classifyTrailEvent("CreateUser", Some("true")) shouldBe None
    CloudAWS.classifyTrailEvent("ConsoleLogin", Some("false")) shouldBe Some(Severity.MEDIUM)
  }

  it should "classify high risk and other mutating events" in {
    CloudAWS.classifyTrailEvent("CreateAccessKey") shouldBe Some(Severity.HIGH)
    CloudAWS.classifyTrailEvent("AttachUserPolicy") shouldBe Some(Severity.HIGH)
    CloudAWS.classifyTrailEvent("StopLogging") shouldBe Some(Severity.HIGH)
    CloudAWS.classifyTrailEvent("TagUser") shouldBe Some(Severity.MEDIUM)
  }

  "CloudAWS.trailErrorCode" should "extract errorCode from CloudTrail event" in {
    CloudAWS.trailErrorCode("""{"eventName":"CreateUser","errorCode":"AccessDenied"}""") shouldBe "AccessDenied"
    CloudAWS.trailErrorCode("""{"eventName":"CreateUser"}""") shouldBe ""
    CloudAWS.trailErrorCode("not json") shouldBe ""
    CloudAWS.trailErrorCode(null) shouldBe ""
  }

  "CloudAWS.fromTrailEvent" should "map CloudTrail event" in {
    val ts = 1700000000000L
    val e = new TrailEvent()
      .withEventId("e-1")
      .withEventName("AttachUserPolicy")
      .withEventSource("iam.amazonaws.com")
      .withUsername("admin")
      .withEventTime(new Date(ts))
      .withReadOnly("false")
      .withCloudTrailEvent("""{"errorCode":"AccessDenied"}""")
      .withResources(new Resource().withResourceName("alice"))

    val c = CloudAWS.fromTrailEvent(e).get
    c.system shouldBe Cloud.SYSTEM_TRAIL
    c.id shouldBe "trail:e-1"
    c.status shouldBe "AttachUserPolicy"
    c.resource shouldBe "alice"
    c.sev shouldBe Severity.HIGH
    c.ts shouldBe ts
    c.meta("user") shouldBe "admin"
    c.meta("source") shouldBe "iam.amazonaws.com"
    c.meta("error_code") shouldBe "AccessDenied"
  }

  it should "skip read-only events" in {
    CloudAWS.fromTrailEvent(new TrailEvent().withEventId("e-2").withEventName("ListUsers")) shouldBe None
  }

  // ---- S3 ----
  "CloudAWS.statusS3" should "map errors" in {
    def ex(code: Int) = { val e = new AmazonServiceException("err"); e.setStatusCode(code); e }
    CloudAWS.statusS3(ex(403)) shouldBe ("ACCESS_DENIED", Severity.MEDIUM)
    CloudAWS.statusS3(ex(404)) shouldBe ("NOT_FOUND", Severity.HIGH)
    CloudAWS.statusS3(ex(503)) shouldBe ("UNAVAILABLE", Severity.CRITICAL)
    CloudAWS.statusS3(ex(400)) shouldBe ("ERROR", Severity.HIGH)
    CloudAWS.statusS3(new SdkClientException("no network")) shouldBe ("UNREACHABLE", Severity.CRITICAL)
  }

  "CloudAWS.alertS3" should "create alert" in {
    val c = CloudAWS.alertS3("bucket-1", "NOT_FOUND", Severity.HIGH, "not found", 12L)
    c.system shouldBe Cloud.SYSTEM_STORAGE
    c.id shouldBe "s3:bucket-1:NOT_FOUND"
    c.resource shouldBe "bucket-1"
    c.meta("latency") shouldBe "12"
  }

  // ---- Cloud ----
  "CloudAWS" should "be created from URI without network access" in {
    val c = new CloudAWS("aws://AKIAKEY:SUPERSECRET@?region=eu-west-1")
    c.provider shouldBe "aws"
    c.uri.region shouldBe Some("eu-west-1")
    c.toString should not include "SUPERSECRET"
    c.close()
  }

  "Cloud" should "create clouds by URI scheme" in {
    Cloud("aws://?region=eu-west-1") shouldBe a[CloudAWS]
    Cloud("gcp://") shouldBe a[CloudGCP]
    Cloud("azure://") shouldBe a[CloudAzure]
    an[IllegalArgumentException] should be thrownBy Cloud("ftp://host")
    an[IllegalArgumentException] should be thrownBy Cloud("")
  }

  it should "normalize system names" in {
    Cloud.system("cloudwatch") shouldBe Some(Cloud.SYSTEM_WATCH)
    Cloud.system("IAM") shouldBe Some(Cloud.SYSTEM_IAM)
    Cloud.system(" Storage ") shouldBe Some(Cloud.SYSTEM_STORAGE)
    Cloud.system("s3") shouldBe Some(Cloud.SYSTEM_STORAGE)
    Cloud.system("unknown") shouldBe None
  }
}
