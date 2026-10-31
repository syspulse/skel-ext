package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.hacken.ext.sentinel.SentryRun
import io.syspulse.skel.plugin.PluginDescriptor
import io.syspulse.ext.sentinel.cloud.{Cloud, CloudAWS, CloudGCP, CloudAzure}

/**
 * Specialized Detectors: DetectorCloudAWS, DetectorCloudGCP, DetectorCloudAzure
 */
class DetectorCloudProvidersSpec extends AnyFlatSpec with Matchers with DetectorCloudFixture {

  "Cloud.system" should "accept Cloud specific names" in {
    Cloud.system("CloudWatch") shouldBe Some("Watch")
    Cloud.system("CloudTrail") shouldBe Some("Trail")
    Cloud.system("S3") shouldBe Some("Storage")
    Cloud.system("IAM") shouldBe Some("IAM")

    Cloud.system("Cloud Monitoring") shouldBe Some("Watch")
    Cloud.system("Cloud Audit Logs") shouldBe Some("Trail")
    Cloud.system("Cloud IAM") shouldBe Some("IAM")
    Cloud.system("Cloud Storage") shouldBe Some("Storage")

    Cloud.system("Azure Monitor") shouldBe Some("Watch")
    Cloud.system("Activity Log") shouldBe Some("Trail")
    Cloud.system("Entra ID") shouldBe Some("IAM")
    Cloud.system("Blob Storage") shouldBe Some("Storage")
  }

  "DetectorCloudAWS" should "use aws:// and CloudWatch by default" in {
    val (detector, conf, rx, r) = start("{}", new DetectorCloudAWS(PluginDescriptor("DetectorCloudAWS", "1.0.0", "DetectorCloudAWS")))
    r shouldBe SentryRun.SENTRY_RUNNING
    detector shouldBe a[DetectorCloudAWS]
    rx.get("cloud").get shouldBe a[CloudAWS]
    rx.get("system").get shouldBe Seq("Watch")
  }

  it should "accept Cloud specific systems" in {
    for (sys <- Seq("CloudWatch", "CloudTrail", "IAM", "S3")) {
      val (detector, conf, rx, r) = start(s"""{ "system": "${sys}" }""", new DetectorCloudAWS(PluginDescriptor("DetectorCloudAWS", "1.0.0", "DetectorCloudAWS")))
      withClue(sys) {
        r shouldBe SentryRun.SENTRY_RUNNING
        rx.get("system").get.asInstanceOf[Seq[String]] should have size 1
      }
    }
  }

  it should "force aws:// scheme" in {
    val (detector, conf, rx, r) = start("""{ "uri": "gcp://?project=p1", "system": "CloudWatch" }""",
      new DetectorCloudAWS(PluginDescriptor("DetectorCloudAWS", "1.0.0", "DetectorCloudAWS")))
    r shouldBe SentryRun.SENTRY_RUNNING
    rx.get("cloud").get shouldBe a[CloudAWS]
    rx.get("cloud").get.asInstanceOf[CloudAWS].uri.cloud shouldBe "aws"
  }

  "DetectorCloudGCP" should "use gcp:// and Cloud Monitoring by default" in {
    val (detector, conf, rx, r) = start("""{ "uri": "gcp://?mock=ok" }""", new DetectorCloudGCP(PluginDescriptor("DetectorCloudGCP", "1.0.0", "DetectorCloudGCP")))
    r shouldBe SentryRun.SENTRY_RUNNING
    detector shouldBe a[DetectorCloudGCP]
    rx.get("cloud").get shouldBe a[CloudGCP]
    rx.get("system").get shouldBe Seq("Watch")
  }

  it should "accept GCP systems and generate Alerts with sys" in {
    val (detector, conf, rx, r) = start("""{ "uri": "gcp://?mock=alarm", "system": "Cloud Audit Logs" }""",
      new DetectorCloudGCP(PluginDescriptor("DetectorCloudGCP", "1.0.0", "DetectorCloudGCP")))
    r shouldBe SentryRun.SENTRY_RUNNING
    rx.get("system").get shouldBe Seq("Trail")

    val events = detector.onCron(rx, 0L)
    events should not be empty
    events.head.metadata("system") shouldBe "Trail"
    events.head.metadata("sys") shouldBe "Cloud Audit Logs"
  }

  "DetectorCloudAzure" should "use azure:// and Azure Monitor by default" in {
    val (detector, conf, rx, r) = start("""{ "uri": "azure://?mock=ok" }""", new DetectorCloudAzure(PluginDescriptor("DetectorCloudAzure", "1.0.0", "DetectorCloudAzure")))
    r shouldBe SentryRun.SENTRY_RUNNING
    detector shouldBe a[DetectorCloudAzure]
    rx.get("cloud").get shouldBe a[CloudAzure]
    rx.get("system").get shouldBe Seq("Watch")
  }

  it should "accept Azure systems" in {
    val (detector, conf, rx, r) = start("""{ "uri": "azure://?mock=alarm", "system": "Blob Storage" }""",
      new DetectorCloudAzure(PluginDescriptor("DetectorCloudAzure", "1.0.0", "DetectorCloudAzure")))
    r shouldBe SentryRun.SENTRY_RUNNING
    rx.get("system").get shouldBe Seq("Storage")

    val events = detector.onCron(rx, 0L)
    events.head.metadata("sys") shouldBe "Blob Storage"
  }
}
