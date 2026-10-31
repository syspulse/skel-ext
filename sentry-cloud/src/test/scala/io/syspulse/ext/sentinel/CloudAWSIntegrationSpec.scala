package io.syspulse.ext.sentinel

import java.util.UUID

import scala.annotation.tailrec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.amazonaws.services.cloudwatch.{AmazonCloudWatch, AmazonCloudWatchClientBuilder}
import com.amazonaws.services.cloudwatch.model.{PutMetricAlarmRequest, SetAlarmStateRequest, DeleteAlarmsRequest}
import com.amazonaws.services.identitymanagement.{AmazonIdentityManagement, AmazonIdentityManagementClientBuilder}
import com.amazonaws.services.identitymanagement.model.{CreateUserRequest, DeleteUserRequest}

import io.hacken.ext.core.{Event, Severity}
import io.hacken.ext.sentinel.SentryRun

/**
 * Detector tests against REAL AWS (CloudWatch, CloudTrail, IAM, S3).
 * 
 * Disabled by default (tests are CANCELED). Enabled with CLOUD_AWS_IT=true and AWS credentials in environment 
 * (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN, AWS_REGION):
 * 
 *   source /tmp/env.dev
 *   CLOUD_AWS_IT=true sbt "sentry_cloud/testOnly *CloudAWSIntegrationSpec"
 * 
 * Tests create temporary resources (prefix `sentry-cloud-it-`) and delete them at the end:
 *  - CloudWatch alarm (forced to ALARM state)
 *  - IAM user (CreateUser/DeleteUser events must appear in CloudTrail)
 * 
 * Required permissions: cloudwatch:PutMetricAlarm/SetAlarmState/DeleteAlarms/DescribeAlarms, 
 *   iam:CreateUser/DeleteUser/GenerateCredentialReport/GetCredentialReport, cloudtrail:LookupEvents, s3:ListAllMyBuckets/ListBucket
 * 
 * CloudTrail events are delayed: test waits up to CLOUD_IT_TRAIL_TIMEOUT minutes (default 20)
 */
class CloudAWSIntegrationSpec extends AnyFlatSpec with Matchers with DetectorCloudFixture {

  val enabled = sys.env.get("CLOUD_AWS_IT").exists(_.equalsIgnoreCase("true")) && 
                sys.env.contains("AWS_ACCESS_KEY_ID") && sys.env.contains("AWS_SECRET_ACCESS_KEY")

  def requireAws() = assume(enabled, "AWS integration tests are disabled: set CLOUD_AWS_IT=true and AWS credentials (source /tmp/env.dev)")

  val REGION = sys.env.getOrElse("AWS_REGION", "eu-west-1")
  val TRAIL_TIMEOUT = sys.env.get("CLOUD_IT_TRAIL_TIMEOUT").flatMap(_.toIntOption).getOrElse(20)

  // credentials from environment (default AWS chain)
  def cloudwatch: AmazonCloudWatch = AmazonCloudWatchClientBuilder.standard().withRegion(REGION).build()
  def iam: AmazonIdentityManagement = AmazonIdentityManagementClientBuilder.standard().withRegion("us-east-1").build()

  def uid = UUID.randomUUID().toString.take(8)

  // credentials are taken by Cloud from environment
  def uri(params: String) = s"aws://?region=${REGION}&${params}"

  @tailrec
  private def poll(detector: DetectorCloud, rx: io.hacken.ext.sentinel.SentinelBlockchains.SentryRun0, 
                   until: Seq[Event] => Boolean, deadline: Long, delay: Long, acc: Seq[Event] = Seq.empty): Seq[Event] = {
    val ee = acc ++ detector.onCron(rx, 0L)
    if (until(ee) || System.currentTimeMillis() > deadline) ee
    else {
      info(s"waiting for CloudTrail events ... (${(deadline - System.currentTimeMillis()) / 1000} sec left)")
      Thread.sleep(delay)
      poll(detector, rx, until, deadline, delay, ee)
    }
  }

  // AWS SSO profile (~/.aws/config) through `credential_process` (bin/aws-credential-process.sh): credentials are requested from AWS CLI by SDK itself.
  //   aws sso login --profile haas-dev
  //   CLOUD_AWS_IT=true CLOUD_AWS_SSO_PROFILE=haas-dev sbt "sentry_cloud/testOnly *CloudAWSIntegrationSpec"
  "DetectorCloud (AWS)" should "access AWS with refreshable credentials from SSO profile" in {
    assume(sys.env.get("CLOUD_AWS_IT").exists(_.equalsIgnoreCase("true")) && sys.env.contains("CLOUD_AWS_SSO_PROFILE"),
      "set CLOUD_AWS_IT=true and CLOUD_AWS_SSO_PROFILE=<sso profile>")
    val sso = sys.env("CLOUD_AWS_SSO_PROFILE")
    // tests may run from the root or from the module directory
    val script = Seq("bin/aws-credential-process.sh", "sentry-cloud/bin/aws-credential-process.sh").map(new java.io.File(_)).find(_.exists()).get

    val file = java.nio.file.Files.createTempFile("sentry-cloud-credentials", "")
    try {
      java.nio.file.Files.writeString(file, s"[sentry]\ncredential_process = ${script.getAbsolutePath} ${sso}\n")

      // no keys in environment are used: they are ignored because `profile` has priority
      val cloud = new io.syspulse.ext.sentinel.cloud.CloudAWS(s"aws://?region=${REGION}", Some(s"profile://${file}?profile=sentry"))
      try {
        cloud.auth shouldBe "profile"
        val alarms = scala.concurrent.Await.result(cloud.checkWatch(), scala.concurrent.duration.Duration(60, "sec"))
        info(s"alarms: ${alarms.size}")
      } finally {
        cloud.close()
      }
    } finally {
      java.nio.file.Files.deleteIfExists(file)
    }
  }

  // credentials come from environment, even if mock variables (AWS_ACCESS_KEY=test) are also defined 
  it should "query CloudTrail with credentials from environment" in {
    requireAws()

    val cloud = new io.syspulse.ext.sentinel.cloud.CloudAWS(uri("source=iam.amazonaws.com&window=60"))
    try {
      val events = scala.concurrent.Await.result(cloud.checkTrail(), scala.concurrent.duration.Duration(60, "sec"))
      info(s"auth=${cloud.auth}, events: ${events.size}")
    } finally {
      cloud.close()
    }
  }

  it should "generate INFO Alerts for read-only CloudTrail events" in {
    requireAws()

    // our own calls (CloudWatch/IAM/CloudTrail LookupEvents) are read-only events 
    val cloud = new io.syspulse.ext.sentinel.cloud.CloudAWS(uri("source=iam.amazonaws.com&window=120"))
    try {
      iam.listUsers()
      val events = scala.concurrent.Await.result(cloud.checkTrail(trackEvent = true), scala.concurrent.duration.Duration(60, "sec"))
      info(s"alerts: ${events.size}")
      events.foreach(_.sev should (be (Severity.INFO) or be (Severity.MEDIUM) or be (Severity.HIGH)))
      // provider event id and console link
      events.foreach { a =>
        a.uid should not be empty
        a.ref should startWith ("https://")
        a.ref should endWith (s"#/events/${a.uid}")
      }
      events.headOption.foreach(a => info(s"ref: ${a.ref}"))
    } finally {
      cloud.close()
    }
  }

  it should "detect CloudWatch alarm" in {
    requireAws()

    val cw = cloudwatch
    val name = s"sentry-cloud-it-${uid}"
    try {
      cw.putMetricAlarm(new PutMetricAlarmRequest()
        .withAlarmName(name).withNamespace("SentryCloudIT").withMetricName("Test").withStatistic("Maximum")
        .withPeriod(60).withEvaluationPeriods(1).withThreshold(1.0).withComparisonOperator("GreaterThanThreshold")
        // alarm stays in ALARM when there is no data
        .withTreatMissingData("breaching"))
      cw.setAlarmState(new SetAlarmStateRequest().withAlarmName(name).withStateValue("ALARM").withStateReason("sentry-cloud integration test"))

      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "${uri(s"alarm_prefix=${name}&states=ALARM")}", "system": "Watch" }""")
      r shouldBe SentryRun.SENTRY_RUNNING

      val events = detector.onCron(rx, 0L)
      events should have size 1
      events.head.metadata("alarm") shouldBe name
      events.head.metadata("state") shouldBe "ALARM"
      events.head.metadata("namespace") shouldBe "SentryCloudIT"
      events.head.severity shouldBe Severity.HIGH

      // the same state is not reported twice
      detector.onCron(rx, 0L) shouldBe empty
    } finally {
      cw.deleteAlarms(new DeleteAlarmsRequest().withAlarmNames(name))
    }
  }

  it should "detect IAM changes from CloudTrail" in {
    requireAws()

    val c = iam
    val user = s"sentry-cloud-it-${uid}"
    try {
      c.createUser(new CreateUserRequest().withUserName(user))
      info(s"created IAM user: ${user}")

      val (detector, conf, rx, r) = start(
        s"""{ "cron": "1 min", "uri": "${uri("source=iam.amazonaws.com&window=60&events=CreateUser,DeleteUser")}", "system": "Trail" }""")
      r shouldBe SentryRun.SENTRY_RUNNING

      val deadline = System.currentTimeMillis() + TRAIL_TIMEOUT * 60L * 1000L
      val events = poll(detector, rx, ee => ee.exists(e => e.metadata("event") == "CreateUser" && e.metadata("resources").contains(user)), deadline, 30000L)

      val created = events.filter(e => e.metadata("event") == "CreateUser" && e.metadata("resources").contains(user))
      created should have size 1
      created.head.metadata("system") shouldBe "Trail"
      created.head.metadata("source") shouldBe "iam.amazonaws.com"
      created.head.metadata("user") should not be empty
      created.head.severity shouldBe Severity.HIGH
      
      // events filter
      events.foreach(e => Set("CreateUser", "DeleteUser") should contain (e.metadata("event")))
    } finally {
      try c.deleteUser(new DeleteUserRequest().withUserName(user)) catch { case e: Exception => info(s"failed to delete ${user}: ${e.getMessage}") }
    }
  }

  it should "detect IAM user deletion from CloudTrail" in {
    requireAws()

    val c = iam
    val user = s"sentry-cloud-it-${uid}"
    c.createUser(new CreateUserRequest().withUserName(user))
    c.deleteUser(new DeleteUserRequest().withUserName(user))
    info(s"created and deleted IAM user: ${user}")

    val (detector, conf, rx, r) = start(
      s"""{ "cron": "1 min", "uri": "${uri("source=iam.amazonaws.com&window=60&events=DeleteUser")}", "system": "Trail" }""")

    val deadline = System.currentTimeMillis() + TRAIL_TIMEOUT * 60L * 1000L
    val events = poll(detector, rx, ee => ee.exists(_.metadata("resources").contains(user)), deadline, 30000L)

    val deleted = events.filter(_.metadata("resources").contains(user))
    deleted should have size 1
    deleted.head.metadata("event") shouldBe "DeleteUser"
  }

  it should "run IAM and Storage checks without errors" in {
    requireAws()

    val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "${uri("")}", "system": "IAM,Storage" }""")
    r shouldBe SentryRun.SENTRY_RUNNING

    val events = detector.onCron(rx, 0L)
    info(s"events: ${events.size}")
    events.filter(_.metadata.contains("err")) shouldBe empty
  }
}
