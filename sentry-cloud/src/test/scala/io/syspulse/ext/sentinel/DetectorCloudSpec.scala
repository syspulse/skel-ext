package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json._

import io.hacken.ext.core.Severity
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Config
import io.syspulse.skel.plugin.PluginDescriptor

import io.syspulse.ext.sentinel.cloud.{Cloud, CloudAlert}

class DetectorCloudSpec extends AnyFlatSpec with Matchers with DetectorCloudFixture {

  // ---- Business logic ----
  "DetectorCloud" should "have default configuration" in {
    DetectorCloud.DEF_CRON shouldBe "10 min"
    DetectorCloud.DEF_URI shouldBe "aws://"
    DetectorCloud.DEF_SYSTEM shouldBe "Watch"
    DetectorCloud.DEF_CONDITION shouldBe "> 0"
    DetectorCloud.DEF_TRACK_NEW shouldBe true
  }

  "DetectorCloud.parseSystems" should "parse and normalize systems" in {
    DetectorCloud.parseSystems("IAM, storage,cloudwatch,CloudTrail") shouldBe (Seq("IAM", "Storage", "Watch", "Trail"), Seq.empty)
    DetectorCloud.parseSystems("s3,Storage") shouldBe (Seq("Storage"), Seq.empty)
    DetectorCloud.parseSystems("IAM,Foo") shouldBe (Seq("IAM"), Seq("Foo"))
    DetectorCloud.parseSystems("") shouldBe (Seq.empty, Seq.empty)
  }

  "DetectorCloud.selectNew" should "select only new findings" in {
    def a(id: String) = CloudAlert("IAM", id, "r", "S", Severity.HIGH, "t")
    val alerts = Seq(a("1"), a("2"), a("3"))
    DetectorCloud.selectNew(alerts, Set("1", "3"), trackNew = true).map(_.id) shouldBe Seq("2")
    DetectorCloud.selectNew(alerts, Set("1", "3"), trackNew = false).map(_.id) shouldBe Seq("1", "2", "3")
    DetectorCloud.selectNew(alerts, Set.empty, trackNew = true) should have size 3
  }

  // ---- Detector from DetectorConfig ----
  it should "create and start detector from config" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?project=p1", "system": "Watch" }""")
    r shouldBe SentryRun.SENTRY_RUNNING
    rx.get("cloud").get shouldBe a[Cloud]
    rx.get("system").get shouldBe Seq("Watch")
  }

  it should "start with AWS Cloud without network access" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "aws://AKIAKEY:SECRET@?region=eu-west-1", "system": "Watch,Trail,IAM,Storage" }""")
    r shouldBe SentryRun.SENTRY_RUNNING
    rx.get("cloud").get.toString should not include "SECRET"
  }

  it should "add region and account to Alerts" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?project=p1&region=europe-west1&account=acc-1&mock=alarm", "system": "Watch" }""")
    r shouldBe SentryRun.SENTRY_RUNNING

    val events = detector.onCron(rx, 0L)
    events should not be empty
    events.foreach { e =>
      e.metadata("region") shouldBe "europe-west1"
      e.metadata("account") shouldBe "acc-1"
    }
  }

  it should "add cloud specific system name to Alerts as `sys`" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "Watch,Trail,IAM,Storage" }""")
    val events = detector.onCron(rx, 0L)
    events.map(e => e.metadata("system") -> e.metadata("sys")).toMap shouldBe Map(
      "Watch" -> "Cloud Monitoring", "Trail" -> "Cloud Audit Logs", "IAM" -> "Cloud IAM", "Storage" -> "Cloud Storage")

    // AWS names
    val (d2, c2, rx2, r2) = start("""{ "cron": "1 min", "uri": "aws://localhost:1?region=us-east-1&ssl=false", "system": "Trail,Storage" }""")
    val errors = d2.onCron(rx2, 0L)
    errors.map(e => e.metadata("system") -> e.metadata("sys")).toMap shouldBe Map("Trail" -> "CloudTrail", "Storage" -> "S3")
  }

  it should "set ref and Cloud provider id (tx_hash) to Alerts" in {
    import scala.concurrent.Future
    import io.syspulse.ext.sentinel.cloud.{CloudAlert, CloudURI}

    class Stub extends Cloud {
      val uri = CloudURI("aws://?region=eu-west-1")
      def provider = "stub"
      def checkWatch() = Future.successful(Seq.empty)
      def checkIAM() = Future.successful(Seq.empty)
      def checkStorage() = Future.successful(Seq.empty)
      def checkTrail(trackEvent: Boolean) = Future.successful(Seq(
        CloudAlert("Trail", "trail:ev-1", "r", "CreateUser", 0.5, "t1", 0L, Map.empty, ref = "https://console/ev-1", uid = "ev-1"),
        // no ref and uid
        CloudAlert("Trail", "trail:ev-2", "r", "CreateUser", 0.5, "t2")
      ))
    }

    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=ok", "system": "Trail" }""")
    rx.set("cloud", new Stub)

    val events = detector.onCron(rx, 0L).sortBy(_.metadata("tx_hash"))
    events should have size 2
    events(0).metadata("ref") shouldBe "https://console/ev-1"
    events(0).metadata("tx_hash") shouldBe "ev-1"
    // finding id if Cloud does not provide the id
    events(1).metadata.get("ref") shouldBe None
    events(1).metadata("tx_hash") shouldBe "trail:ev-2"
    // eid is not the provider id
    events(0).eid should not be "ev-1"
    events.foreach(_.metadata.get("id") shouldBe None)
  }

  it should "create Cloud with auth which overrides credentials of uri" in {
    import io.syspulse.ext.sentinel.cloud.CloudAWS

    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "aws://URIKEY:URISECRET:URITOKEN@?region=eu-west-1", "system": "Watch", "auth": "secret://AUTHKEY:AUTHSECRET" }""")
    r shouldBe SentryRun.SENTRY_RUNNING
    val cloud = rx.get("cloud").get.asInstanceOf[CloudAWS]
    cloud.auth shouldBe "secret+sts"
    cloud.uri.region shouldBe Some("eu-west-1")
    cloud.toString should not include "AUTHSECRET"
    cloud.toString should not include "URISECRET"

    // without auth: credentials of uri
    val (d2, c2, rx2, r2) = start("""{ "cron": "1 min", "uri": "aws://URIKEY:URISECRET:URITOKEN@?region=eu-west-1", "system": "Watch" }""")
    rx2.get("cloud").get.asInstanceOf[CloudAWS].auth shouldBe "uri"

    // empty auth is ignored
    val (d3, c3, rx3, r3) = start("""{ "cron": "1 min", "uri": "aws://URIKEY:URISECRET@?region=eu-west-1", "system": "Watch", "auth": "" }""")
    r3 shouldBe SentryRun.SENTRY_RUNNING
    rx3.get("cloud").get.asInstanceOf[CloudAWS].auth shouldBe "uri+sts"
  }

  it should "support auth types" in {
    for ((auth, name) <- Seq(
      "role://" -> "role",
      "profile://?profile=sentry" -> "profile",
      "secret://K:S:T" -> "secret",
      "secret://K:S?arn=arn:aws:iam::123456789012:role/Sentry" -> "role(secret)"
    )) {
      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "aws://?region=eu-west-1", "system": "Watch", "auth": "${auth}" }""")
      r shouldBe SentryRun.SENTRY_RUNNING
      rx.get("cloud").get.asInstanceOf[io.syspulse.ext.sentinel.cloud.CloudAWS].auth shouldBe name
    }
  }

  it should "not start with invalid auth" in {
    for (auth <- Seq("ftp://host", "not-a-uri", "secret://onlykey", "env://$UNDEFINED_VAR_1:$UNDEFINED_VAR_2")) {
      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "aws://?region=eu-west-1", "system": "Watch", "auth": "${auth}" }""")
      withClue(auth) { r shouldBe SentryRun.SENTRY_STOPPED }
    }
  }

  it should "track_event is disabled by default" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=ok", "system": "Trail" }""")
    r shouldBe SentryRun.SENTRY_RUNNING
    rx.get("track_event").get shouldBe false
  }

  it should "pass track_event to Cloud Trail check" in {
    import scala.concurrent.Future
    import io.syspulse.ext.sentinel.cloud.{CloudAlert, CloudURI}

    class Spy extends Cloud {
      var tracked: Option[Boolean] = None
      val uri = CloudURI("aws://")
      def provider = "spy"
      def checkWatch() = Future.successful(Seq.empty)
      def checkIAM() = Future.successful(Seq.empty)
      def checkStorage() = Future.successful(Seq.empty)
      def checkTrail(trackEvent: Boolean) = { tracked = Some(trackEvent); Future.successful(Seq.empty) }
    }

    for (flag <- Seq(true, false)) {
      val (detector, conf, rx, r) = start(s"""{ "cron": "1 min", "uri": "gcp://?mock=ok", "system": "Trail", "track_event": ${flag} }""")
      r shouldBe SentryRun.SENTRY_RUNNING
      rx.get("track_event").get shouldBe flag

      val spy = new Spy
      rx.set("cloud", spy)
      detector.onCron(rx, 0L) shouldBe empty
      spy.tracked shouldBe Some(flag)
    }
  }

  it should "use default uri and system" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min" }""")
    r shouldBe SentryRun.SENTRY_RUNNING
    rx.get("system").get shouldBe Seq("Watch")
  }

  it should "stop on invalid system" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://", "system": "Foo" }""")
    r shouldBe SentryRun.SENTRY_STOPPED
  }

  it should "stop on invalid uri" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "ftp://host", "system": "IAM" }""")
    r shouldBe SentryRun.SENTRY_STOPPED
  }

  it should "create new Cloud on update" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://", "system": "IAM" }""")
    val cloud1 = rx.get("cloud").get.asInstanceOf[AnyRef]
    detector.onUpdate(rx, conf) shouldBe SentryRun.SENTRY_RUNNING
    val cloud2 = rx.get("cloud").get.asInstanceOf[AnyRef]
    cloud1 should not be theSameInstanceAs(cloud2)
  }

  // ---- onCron ----
  it should "generate alerts on cron" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?project=p1&mock=alarm", "system": "Watch,Trail,IAM,Storage" }""")
    val events = detector.onCron(rx, 0L)

    events should have size 4
    events.foreach { e =>
      e.did shouldBe "DetectorCloud"
      e.metadata("provider") shouldBe "gcp"
      e.metadata.contains("system") shouldBe true
      e.metadata.contains("title") shouldBe true
      // description template is resolved with metadata
      e.metadata("desc") shouldBe s"${e.metadata("system")}: ${e.metadata("title")}"
    }
    events.map(_.metadata("system")).toSet shouldBe Set("Watch", "Trail", "IAM", "Storage")
    events.map(_.severity).toSet should contain (Severity.CRITICAL)
  }

  it should "alert only on new findings" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "IAM" }""")
    detector.onCron(rx, 0L) should have size 1
    detector.onCron(rx, 0L) shouldBe empty
    detector.onCron(rx, 0L) shouldBe empty
  }

  it should "generate idempotent eid" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "Watch,Trail,IAM,Storage", "track_new": false }""")
    val e1 = detector.onCron(rx, 0L)
    val e2 = detector.onCron(rx, 0L)
    e1 should have size 4
    // same alerts have the same eid
    e1.map(_.eid) shouldBe e2.map(_.eid)
    // different alerts have different eid
    e1.map(_.eid).toSet should have size 4
    e1.foreach(e => e.eid shouldBe detector.eid(rx, CloudAlert(e.metadata("system"), e.metadata("tx_hash"), "", "", 0.0, "")))

    // the same configuration in another Detector instance 
    val (detector2, conf2, rx2, r2) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "Watch,Trail,IAM,Storage", "track_new": false }""")
    detector2.onCron(rx2, 0L).map(_.eid) shouldBe e1.map(_.eid)
  }

  it should "generate idempotent eid after finding is resolved and appears again" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "IAM" }""")
    val e1 = detector.onCron(rx, 0L)
    detector.onCron(rx, 0L) shouldBe empty
    // resolved
    rx.set("seen_IAM", Set.empty[String])
    val e2 = detector.onCron(rx, 0L)
    e2.map(_.eid) shouldBe e1.map(_.eid)
  }

  it should "alert on every check if track_new is disabled" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "IAM", "track_new": false }""")
    detector.onCron(rx, 0L) should have size 1
    detector.onCron(rx, 0L) should have size 1
  }

  it should "not alert if everything is healthy" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "azure://?mock=ok", "system": "Watch,Trail,IAM,Storage" }""")
    detector.onCron(rx, 0L) shouldBe empty
  }

  it should "apply condition to number of findings" in {
    // 1 finding per system: condition `> 1` is not satisfied
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "azure://?mock=alarm", "system": "IAM", "condition": "> 1" }""")
    detector.onCron(rx, 0L) shouldBe empty
  }

  it should "use custom description" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "azure://?mock=alarm", "system": "IAM", "desc": "Cloud problem: {title}" }""")
    detector.onCron(rx, 0L).head.metadata("desc") shouldBe "Cloud problem: User was assigned Owner role"
  }

  it should "generate error alert when Cloud fails" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=fail", "system": "Storage" }""")
    val events = detector.onCron(rx, 0L)
    events should have size 1
    events.head.metadata("err") should include("mocked failure")
    events.head.metadata("system") shouldBe "Storage"
    // errors are always tracked by default
    detector.onCron(rx, 0L) should have size 1
  }

  it should "not generate error alerts if track_err is disabled" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=fail", "system": "Storage", "track_err": false }""")
    detector.onCron(rx, 0L) shouldBe empty
  }

  it should "return nothing if Cloud is not created" in {
    val (detector, conf, rx) = createRun("""{ "cron": "1 min" }""")
    detector.onInit(rx, conf)
    detector.onCron(rx, 0L) shouldBe empty
  }
}
