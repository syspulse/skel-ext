package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.matching.Regex
import spray.json._
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Config
import io.syspulse.skel.plugin.PluginDescriptor
import io.syspulse.ext.sentinel.feeds.{NewsPost, MeltwaterFeed}
import io.hacken.ext.sentinel.ThresholdDouble

class DetectorMeltwaterSpec extends AnyFlatSpec with Matchers {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private implicit val config: Config = Config()

  private val meltwaterCsv: String = getClass.getResource("/meltwater/Examples/export-news-1.csv").getPath
  private val meltwaterTwitterCsv: String = getClass.getResource("/meltwater/Examples/export-twitter-1.csv").getPath
  private val meltwaterFacebookCsv: String = getClass.getResource("/meltwater/Examples/export-facebook-1.csv").getPath

  private def createRun(confJson: String): (DetectorMeltwater, DetectorConfig, SentryRun0) = {
    val conf = DetectorConfig(
      id = 1,
      createdAt = System.currentTimeMillis(),
      updatedAt = System.currentTimeMillis(),
      status = "active",
      contract = io.hacken.ext.detector.DetectorConfigContract(
        id = 1,
        createdAt = System.currentTimeMillis(),
        updatedAt = System.currentTimeMillis(),
        projectId = 1,
        tenantId = 1,
        chainUid = None,
        proxyAddress = None,
        implementation = None,
        address = None,
        name = "Meltwater-Feed"
      ),
      schema = None,
      name = "DetectorMeltwater",
      source = "test",
      tags = Seq.empty,
      config = Some(confJson.parseJson.asJsObject),
      destinations = Seq.empty
    )

    val pd = PluginDescriptor("DetectorMeltwater", "1.0.0", "DetectorMeltwater")
    val detector = new DetectorMeltwater(pd)

    val rx = new SentryRun(detector, conf, new Config { override val env: String = "test" }, None)
    (detector, conf, rx)
  }

  "DetectorFeed.parseFeedUri" should "parse Meltwater URI when type is 'meltwater'" in {
    val (feedType, cleanedUri) = DetectorFeed.parseFeedUri("./meltwater/feed.csv", "meltwater")
    feedType shouldBe "meltwater"
    cleanedUri shouldBe "./meltwater/feed.csv"
  }

  it should "strip meltwater:// prefix when type is empty" in {
    val (feedType, cleanedUri) = DetectorFeed.parseFeedUri("meltwater://./meltwater/feed.csv", "")
    feedType shouldBe "meltwater"
    cleanedUri shouldBe "./meltwater/feed.csv"
  }

  it should "strip meltwater:// prefix with file:// URI" in {
    val (feedType, cleanedUri) = DetectorFeed.parseFeedUri("meltwater://file://./meltwater/feed.csv", "")
    feedType shouldBe "meltwater"
    cleanedUri shouldBe "file://./meltwater/feed.csv"
  }

  "DetectorMeltwater" should "create detector from config" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 100,
      "max_seen_posts": 100
    }"""
    val (detector, conf, rx) = createRun(confJson)
    val result = detector.onInit(rx, conf)
    result shouldBe SentryRun.SENTRY_INIT
  }

  it should "initialize with Meltwater feed from config" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 100,
      "max_seen_posts": 100
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val feeds = rx.get("feeds").get.asInstanceOf[Seq[io.syspulse.ext.sentinel.feeds.NewsFeed]]
    feeds should have size 1
    feeds.head shouldBe a[MeltwaterFeed]
    feeds.head.getSourceType() shouldBe "meltwater"
  }

  it should "generate alerts from Meltwater feed" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 5,
      "max_seen_posts": 100
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events = detector.onCron(rx, 0L)

    events should not be empty
    events.foreach { event =>
      event.did shouldBe "DetectorMeltwater"
      event.metadata should not be empty
    }
  }

  it should "generate alerts from Meltwater Twitter export" in {
    // Match-all script: Twitter rows often have empty Headline/Opening Text; default filter would drop them.
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterTwitterCsv",
      "max": 5,
      "max_seen_posts": 100,
      "script": [ { "type": "regexp_score", "src": "(?s).*" } ]
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events = detector.onCron(rx, 0L)

    events should not be empty
    events.foreach { event =>
      event.did shouldBe "DetectorMeltwater"
      event.metadata should not be empty
      event.metadata.get("Source").map(_.toString) shouldBe Some("twitter")
    }
  }

  it should "generate alerts from Meltwater Facebook export" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterFacebookCsv",
      "max": 5,
      "max_seen_posts": 100,
      "script": [ { "type": "regexp_score", "src": "(?s).*" } ]
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events = detector.onCron(rx, 0L)

    events should not be empty
    events.foreach { event =>
      event.did shouldBe "DetectorMeltwater"
      event.metadata should not be empty
      event.metadata.get("Source").map(_.toString) shouldBe Some("facebook")
    }
  }

  it should "apply source filter" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 100,
      "max_seen_posts": 100,
      "source": "Open PR"
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events = detector.onCron(rx, 0L)

    // All events should be from "Open PR" source
    events.foreach { event =>
      val metadata = event.metadata
      metadata.contains("Source") shouldBe true
      metadata("Source").toString should include("Open PR")
    }
  }

  it should "apply country filter" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 100,
      "max_seen_posts": 100,
      "country": "us,de"
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events = detector.onCron(rx, 0L)

    // All events should be from US or DE
    events.foreach { event =>
      val metadata = event.metadata
      metadata.contains("Country") shouldBe true
      val country = metadata("Country").toString.toLowerCase
      (country should (include("us") or include("de")))
    }
  }

  it should "apply language filter" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 100,
      "max_seen_posts": 100,
      "language": "en"
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events = detector.onCron(rx, 0L)

    // All events should be in English
    events.foreach { event =>
      val metadata = event.metadata
      metadata.contains("Language") shouldBe true
      metadata("Language").toString shouldBe "en"
    }
  }

  it should "apply reach filter" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 100,
      "max_seen_posts": 100,
      "reach": ">100000"
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events = detector.onCron(rx, 0L)

    // All events should have reach > 100000
    events.foreach { event =>
      val metadata = event.metadata
      metadata.contains("Reach") shouldBe true
      val reach = metadata("Reach").toString.toDouble
      reach should be > 100000.0
    }
  }

  it should "not track duplicate posts" in {
    val confJson = s"""{
      "cron": "5000",
      "type": "meltwater",
      "feeds": "$meltwaterCsv",
      "max": 100,
      "max_seen_posts": 100
    }"""
    val (detector, conf, rx) = createRun(confJson)
    detector.onInit(rx, conf)
    detector.onStart(rx, conf)

    val events1 = detector.onCron(rx, 0L)
    val events2 = detector.onCron(rx, 0L)

    events1 should not be empty
    events2 shouldBe empty  // Second run should find no new posts
  }

  "DetectorMeltwater.matchesFilters" should "match posts with source filter" in {
    val post = NewsPost(
      id = "test",
      title = "Test",
      link = "http://test.com",
      author = "Test",
      publishedDate = System.currentTimeMillis(),
      summary = "Test",
      source = "test",
      typ = "meltwater",
      categories = List.empty,
      images = List.empty,
      feedMetadata = Map("Source" -> "Open PR")
    )

    val result = DetectorMeltwater.matchesFilters(
      post,
      Seq("Open PR"),
      Seq.empty,
      Seq.empty,
      Seq.empty,
      None,
      None,
      Seq.empty
    )

    result shouldBe true
  }

  it should "not match posts that fail source filter" in {
    val post = NewsPost(
      id = "test",
      title = "Test",
      link = "http://test.com",
      author = "Test",
      publishedDate = System.currentTimeMillis(),
      summary = "Test",
      source = "test",
      typ = "meltwater",
      categories = List.empty,
      images = List.empty,
      feedMetadata = Map("Source" -> "Other Source")
    )

    val result = DetectorMeltwater.matchesFilters(
      post,
      Seq("Open PR"),
      Seq.empty,
      Seq.empty,
      Seq.empty,
      None,
      None,
      Seq.empty
    )

    result shouldBe false
  }

  it should "match posts with reach threshold" in {
    val post = NewsPost(
      id = "test",
      title = "Test",
      link = "http://test.com",
      author = "Test",
      publishedDate = System.currentTimeMillis(),
      summary = "Test",
      source = "test",
      typ = "meltwater",
      categories = List.empty,
      images = List.empty,
      feedMetadata = Map("Reach" -> "150000")
    )

    val threshold = new ThresholdDouble(0.0, ">100000")
    val result = DetectorMeltwater.matchesFilters(
      post,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Some(threshold),
      None,
      Seq.empty
    )

    result shouldBe true
  }

  it should "not match posts that fail reach threshold" in {
    val post = NewsPost(
      id = "test",
      title = "Test",
      link = "http://test.com",
      author = "Test",
      publishedDate = System.currentTimeMillis(),
      summary = "Test",
      source = "test",
      typ = "meltwater",
      categories = List.empty,
      images = List.empty,
      feedMetadata = Map("Reach" -> "50000")
    )

    val threshold = new ThresholdDouble(0.0, ">100000")
    val result = DetectorMeltwater.matchesFilters(
      post,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Some(threshold),
      None,
      Seq.empty
    )

    result shouldBe false
  }
}
