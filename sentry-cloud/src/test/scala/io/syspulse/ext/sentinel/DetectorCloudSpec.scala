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
    DetectorCloud.DEF_NEW_ONLY shouldBe true
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
    DetectorCloud.selectNew(alerts, Set("1", "3"), newOnly = true).map(_.id) shouldBe Seq("2")
    DetectorCloud.selectNew(alerts, Set("1", "3"), newOnly = false).map(_.id) shouldBe Seq("1", "2", "3")
    DetectorCloud.selectNew(alerts, Set.empty, newOnly = true) should have size 3
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
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "Watch,Trail,IAM,Storage", "new_only": false }""")
    val e1 = detector.onCron(rx, 0L)
    val e2 = detector.onCron(rx, 0L)
    e1 should have size 4
    // same alerts have the same eid
    e1.map(_.eid) shouldBe e2.map(_.eid)
    // different alerts have different eid
    e1.map(_.eid).toSet should have size 4
    e1.foreach(e => e.eid shouldBe detector.eid(rx, CloudAlert(e.metadata("system"), e.metadata("id"), "", "", 0.0, "")))

    // the same configuration in another Detector instance 
    val (detector2, conf2, rx2, r2) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "Watch,Trail,IAM,Storage", "new_only": false }""")
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

  it should "alert on every check if new_only is disabled" in {
    val (detector, conf, rx, r) = start("""{ "cron": "1 min", "uri": "gcp://?mock=alarm", "system": "IAM", "new_only": false }""")
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
