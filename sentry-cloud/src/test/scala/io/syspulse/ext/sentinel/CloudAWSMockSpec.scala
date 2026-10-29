package io.syspulse.ext.sentinel

import java.net.{HttpURLConnection, URL}
import java.util.UUID

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials}
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
import com.amazonaws.services.cloudwatch.{AmazonCloudWatch, AmazonCloudWatchClientBuilder}
import com.amazonaws.services.cloudwatch.model.{PutMetricAlarmRequest, SetAlarmStateRequest, DeleteAlarmsRequest}
import com.amazonaws.services.identitymanagement.{AmazonIdentityManagement, AmazonIdentityManagementClientBuilder}
import com.amazonaws.services.identitymanagement.model.{CreateUserRequest, CreateLoginProfileRequest, DeleteLoginProfileRequest, DeleteUserRequest}
import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentryRun

/**
 * Detector tests against mocked AWS services (moto): CloudWatch, IAM, S3.
 * 
 * Mock is started with: ./mock/moto.sh start
 * Tests are CANCELED if mock is not reachable (AWS_MOCK_URL, default: http://localhost:5000)
 * 
 * Tests delete created resources at the end (they live for milliseconds). To see Alerts in a running Sentinel
 * (../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Moto-1.conf) keep them: CLOUD_MOCK_KEEP=true
 * 
 * moto does not support CloudTrail LookupEvents: Trail is tested in CloudAWSIntegrationSpec (real AWS)
 */
class CloudAWSMockSpec extends AnyFlatSpec with Matchers with DetectorCloudFixture {

  val MOCK_URL = sys.env.getOrElse("AWS_MOCK_URL", "http://localhost:5000")
  val REGION = "us-east-1"

  lazy val mockUp: Boolean = try {
    val c = new URL(s"${MOCK_URL}/moto-api/").openConnection().asInstanceOf[HttpURLConnection]
    c.setConnectTimeout(1000)
    c.setReadTimeout(1000)
    c.getResponseCode == 200
  } catch { case _: Exception => false }

  // keep created resources (alarms, users, buckets) in mock to see them in running Sentinel
  val keep = sys.env.get("CLOUD_MOCK_KEEP").exists(_.equalsIgnoreCase("true"))

  def requireMock() = assume(mockUp, s"AWS mock is not running: ${MOCK_URL} (start: ./mock/moto.sh start)")

  // dummy credentials are in URI: it is a mock
  def uri(params: String) = {
    val host = MOCK_URL.replaceFirst("^https?://", "")
    s"aws://test:test@${host}?region=${REGION}&ssl=${MOCK_URL.startsWith("https")}&${params}"
  }

  private def ep = new EndpointConfiguration(MOCK_URL, REGION)
  private val creds = new AWSStaticCredentialsProvider(new BasicAWSCredentials("test", "test"))

  def cloudwatch: AmazonCloudWatch = AmazonCloudWatchClientBuilder.standard().withCredentials(creds).withEndpointConfiguration(ep).build()
  def iam: AmazonIdentityManagement = AmazonIdentityManagementClientBuilder.standard().withCredentials(creds).withEndpointConfiguration(ep).build()
  def s3: AmazonS3 = AmazonS3ClientBuilder.standard().withCredentials(creds).withEndpointConfiguration(ep).withPathStyleAccessEnabled(true).build()

  def uid = UUID.randomUUID().toString.take(8)

  "DetectorCloud (AWS mock)" should "detect CloudWatch alarm" in {
    requireMock()

    val cw = cloudwatch
    val name = s"it-alarm-${uid}"
    try {
      cw.putMetricAlarm(new PutMetricAlarmRequest()
        .withAlarmName(name).withNamespace("AWS/EC2").withMetricName("StatusCheckFailed").withStatistic("Maximum")
        .withPeriod(60).withEvaluationPeriods(1).withThreshold(0.0).withComparisonOperator("GreaterThanThreshold"))

      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "${uri(s"alarm_prefix=${name}")}", "system": "Watch" }""")
      r shouldBe SentryRun.SENTRY_RUNNING

      // OK (initial state is INSUFFICIENT_DATA): nothing
      detector.onCron(rx, 0L) shouldBe empty

      cw.setAlarmState(new SetAlarmStateRequest().withAlarmName(name).withStateValue("ALARM").withStateReason("test"))

      val events = detector.onCron(rx, 0L)
      events should have size 1
      events.head.did shouldBe "DetectorCloud"
      events.head.metadata("alarm") shouldBe name
      events.head.metadata("state") shouldBe "ALARM"
      events.head.metadata("system") shouldBe "Watch"
      events.head.metadata("namespace") shouldBe "AWS/EC2"
      events.head.metadata("reason") shouldBe "test"
      events.head.severity shouldBe Severity.HIGH

      // already reported
      detector.onCron(rx, 0L) shouldBe empty

      // recovered and failed again: new alert
      cw.setAlarmState(new SetAlarmStateRequest().withAlarmName(name).withStateValue("OK").withStateReason("recovered"))
      detector.onCron(rx, 0L) shouldBe empty
      Thread.sleep(1100)
      cw.setAlarmState(new SetAlarmStateRequest().withAlarmName(name).withStateValue("ALARM").withStateReason("again"))
      val events2 = detector.onCron(rx, 0L)
      events2 should have size 1
      events2.head.metadata("reason") shouldBe "again"
    } finally {
      if (!keep) cw.deleteAlarms(new DeleteAlarmsRequest().withAlarmNames(name))
    }
  }

  it should "request SESSION_TOKEN for keys without token" in {
    requireMock()

    class TestAWS(u: String) extends io.syspulse.ext.sentinel.cloud.CloudAWS(u) {
      def creds = credentials
    }

    val c = new TestAWS(uri(""))
    try {
      c.auth shouldBe "uri+sts"
      val cred = c.creds.getCredentials
      cred shouldBe a [com.amazonaws.auth.AWSSessionCredentials]
      cred.asInstanceOf[com.amazonaws.auth.AWSSessionCredentials].getSessionToken should not be empty
      // keys are temporary, not the ones from URI
      cred.getAWSAccessKeyId should not be "test"
    } finally c.close()

    // disabled: keys are used as is
    val c2 = new TestAWS(uri("sts=false"))
    try {
      c2.auth shouldBe "uri"
      c2.creds.getCredentials.getAWSAccessKeyId shouldBe "test"
    } finally c2.close()
  }

  it should "detect CloudWatch alarm with assumed role (refreshable credentials)" in {
    requireMock()

    val cw = cloudwatch
    val name = s"it-role-${uid}"
    try {
      cw.putMetricAlarm(new PutMetricAlarmRequest()
        .withAlarmName(name).withNamespace("AWS/EC2").withMetricName("StatusCheckFailed").withStatistic("Maximum")
        .withPeriod(60).withEvaluationPeriods(1).withThreshold(0.0).withComparisonOperator("GreaterThanThreshold"))
      cw.setAlarmState(new SetAlarmStateRequest().withAlarmName(name).withStateValue("ALARM").withStateReason("role"))

      // base credentials (mock) assume the role through STS (mock). Credentials are refreshed by AWS SDK
      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "${uri(s"alarm_prefix=${name}&role_arn=arn:aws:iam::123456789012:role/Sentry&external_id=ext-1")}", "system": "Watch" }""")
      r shouldBe SentryRun.SENTRY_RUNNING

      val events = detector.onCron(rx, 0L)
      events should have size 1
      events.head.metadata("alarm") shouldBe name
    } finally {
      if (!keep) cw.deleteAlarms(new DeleteAlarmsRequest().withAlarmNames(name))
    }
  }

  it should "filter CloudWatch alarms by namespace and state" in {
    requireMock()

    val cw = cloudwatch
    val name = s"it-ns-${uid}"
    try {
      cw.putMetricAlarm(new PutMetricAlarmRequest()
        .withAlarmName(name).withNamespace("AWS/S3").withMetricName("m").withStatistic("Maximum")
        .withPeriod(60).withEvaluationPeriods(1).withThreshold(0.0).withComparisonOperator("GreaterThanThreshold"))
      cw.setAlarmState(new SetAlarmStateRequest().withAlarmName(name).withStateValue("ALARM").withStateReason("test"))

      val (d1, _, rx1, _) = start(s"""{ "cron": "1 min", "uri": "${uri(s"alarm_prefix=${name}&namespace=AWS/EC2,AWS/EKS")}", "system": "Watch" }""")
      d1.onCron(rx1, 0L) shouldBe empty

      val (d2, _, rx2, _) = start(s"""{ "cron": "1 min", "uri": "${uri(s"alarm_prefix=${name}&namespace=AWS/S3")}", "system": "Watch" }""")
      d2.onCron(rx2, 0L) should have size 1

      // INSUFFICIENT_DATA is not reported by default, but ALARM only
      cw.setAlarmState(new SetAlarmStateRequest().withAlarmName(name).withStateValue("INSUFFICIENT_DATA").withStateReason("no data"))
      val (d3, _, rx3, _) = start(s"""{ "cron": "1 min", "uri": "${uri(s"alarm_prefix=${name}")}", "system": "Watch" }""")
      d3.onCron(rx3, 0L) shouldBe empty

      val (d4, _, rx4, _) = start(s"""{ "cron": "1 min", "uri": "${uri(s"alarm_prefix=${name}&states=INSUFFICIENT_DATA")}", "system": "Watch" }""")
      val ee = d4.onCron(rx4, 0L)
      ee should have size 1
      ee.head.severity shouldBe Severity.LOW
    } finally {
      if (!keep) cw.deleteAlarms(new DeleteAlarmsRequest().withAlarmNames(name))
    }
  }

  it should "detect IAM user with console access and without MFA" in {
    requireMock()

    val c = iam
    val user = s"it-user-${uid}"
    try {
      c.createUser(new CreateUserRequest().withUserName(user))
      c.createLoginProfile(new CreateLoginProfileRequest().withUserName(user).withPassword("Passw0rd!x1"))

      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "${uri("")}", "system": "IAM" }""")
      r shouldBe SentryRun.SENTRY_RUNNING

      val events = detector.onCron(rx, 0L)
      val e = events.filter(_.metadata.get("user").contains(user))
      e should have size 1
      e.head.metadata("check") shouldBe "mfa"
      e.head.metadata("system") shouldBe "IAM"
      e.head.severity shouldBe Severity.MEDIUM

      // not reported again
      detector.onCron(rx, 0L).filter(_.metadata.get("user").contains(user)) shouldBe empty
    } finally if (!keep) {
      Seq(() => c.deleteLoginProfile(new DeleteLoginProfileRequest().withUserName(user)), 
          () => c.deleteUser(new DeleteUserRequest().withUserName(user))).foreach(f => try f() catch { case _: Exception => })
    }
  }

  it should "detect unavailable S3 bucket" in {
    requireMock()

    val c = s3
    val ok = s"it-bucket-${uid}"
    val missing = s"it-missing-${uid}"
    try {
      c.createBucket(ok)

      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "${uri(s"buckets=${ok},${missing}")}", "system": "Storage" }""")
      r shouldBe SentryRun.SENTRY_RUNNING

      val events = detector.onCron(rx, 0L)
      events should have size 1
      events.head.metadata("bucket") shouldBe missing
      events.head.metadata("status") shouldBe "NOT_FOUND"
      events.head.metadata("system") shouldBe "Storage"
      events.head.severity shouldBe Severity.HIGH

      detector.onCron(rx, 0L) shouldBe empty

      // all buckets (healthy ones are not reported)
      val (d2, _, rx2, _) = start(s"""{ "cron": "1 min", "uri": "${uri(s"bucket_prefix=it-bucket-${ok.takeRight(8)}")}", "system": "Storage" }""")
      d2.onCron(rx2, 0L) shouldBe empty
    } finally {
      if (!keep) try c.deleteBucket(ok) catch { case _: Exception => }
    }
  }

  it should "report error if AWS is not reachable" in {
    // nothing is listening on this port: error event, not an exception
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "aws://test:test@localhost:1?region=us-east-1&ssl=false", "system": "Watch", "timeout": 20000 }""")
    r shouldBe SentryRun.SENTRY_RUNNING
    val events = detector.onCron(rx, 0L)
    events should have size 1
    events.head.metadata.contains("err") shouldBe true
    events.head.metadata("system") shouldBe "Watch"
  }
}
