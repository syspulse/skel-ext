package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Success, Failure}
import scala.util.Try
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

import io.syspulse.ext.sentinel.feeds.MeltwaterFeed
import io.hacken.ext.sentinel.Config

class MeltwaterFeedSpec extends AnyFlatSpec with Matchers {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private implicit val config: Config = Config()

  private def awaitFeed(feed: MeltwaterFeed, timeoutMs: Long = 0L): Try[Seq[io.syspulse.ext.sentinel.feeds.NewsPost]] =
    Try(Await.result(feed.fetchFeed(timeoutMs)(ec), (config.detectorTimeout + 1000L).millis))

  private def getResourcePath(resource: String): String = {
    getClass.getResource(resource).getPath
  }

  // CSV files are now addressed with the csv:// prefix (JSON is the primary/default format).
  private def csvResource(resource: String): String = "csv://" + getResourcePath(resource)
  private def jsonResource(resource: String): String = getResourcePath(resource)

  "MeltwaterFeed" should "parse Meltwater CSV feed from file" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val result = awaitFeed(feed)

    result shouldBe a[Success[_]]
    val posts = result.get
    posts should not be empty

    val firstPost = posts.head
    firstPost.id should not be empty
    firstPost.title should not be empty
    firstPost.publishedDate should be > 0L
    firstPost.typ shouldBe "meltwater"
  }

  it should "parse Meltwater Twitter export CSV from file" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-twitter-1.csv"))
    val result = awaitFeed(feed)

    result shouldBe a[Success[_]]
    val posts = result.get
    posts should have size 100

    posts.foreach { post =>
      post.id should not be empty
      post.typ shouldBe "meltwater"
      post.publishedDate should be > 0L
      post.feedMetadata.get("Source") shouldBe Some("twitter")
    }
  }

  it should "parse Meltwater Facebook export CSV from file" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-facebook-1.csv"))
    val result = awaitFeed(feed)

    result shouldBe a[Success[_]]
    val posts = result.get
    posts should have size 100

    posts.foreach { post =>
      post.id should not be empty
      post.typ shouldBe "meltwater"
      post.publishedDate should be > 0L
      post.feedMetadata.get("Source") shouldBe Some("facebook")
    }
  }

  it should "extract metadata from Meltwater CSV" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val posts = awaitFeed(feed).get

    posts should not be empty

    posts.foreach { post =>
      // All Meltwater posts should have metadata
      post.feedMetadata should not be empty

      // Check for key Meltwater fields
      post.feedMetadata.contains("Source") shouldBe true
      post.feedMetadata.contains("Country") shouldBe true
      post.feedMetadata.contains("Language") shouldBe true
      post.feedMetadata.contains("Reach") shouldBe true
    }
  }

  it should "parse date field correctly" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val posts = awaitFeed(feed).get

    posts.foreach { post =>
      // Date should be parsed to epoch milliseconds
      post.publishedDate should be > 0L
      // Date should be in 2021 (1609459200000 = Jan 1, 2021; 1640995200000 = Jan 1, 2022)
      post.publishedDate should be >= 1609459200000L
      post.publishedDate should be < 1640995200000L
    }
  }

  it should "extract author from Influencer field" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val posts = awaitFeed(feed).get

    posts should not be empty

    // Check that author field is populated (may be empty if Influencer field is empty)
    posts.foreach { post =>
      post.author should not be null
    }
  }

  it should "include all CSV fields except Date in metadata" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val posts = awaitFeed(feed).get

    posts should not be empty

    val firstPost = posts.head

    // Check that Date is NOT in metadata
    firstPost.feedMetadata.contains("Date") shouldBe false

    // Check that other key fields ARE in metadata
    firstPost.feedMetadata.contains("Headline") shouldBe true
    firstPost.feedMetadata.contains("URL") shouldBe true
    firstPost.feedMetadata.contains("Source") shouldBe true
    firstPost.feedMetadata.contains("Country") shouldBe true
    firstPost.feedMetadata.contains("Language") shouldBe true
    firstPost.feedMetadata.contains("Reach") shouldBe true
    firstPost.feedMetadata.contains("Twitter_Social_Echo") shouldBe true
    firstPost.feedMetadata.contains("Facebook_Social_Echo") shouldBe true
    firstPost.feedMetadata.contains("Reddit_Social_Echo") shouldBe true
    firstPost.feedMetadata.contains("Document_Tags") shouldBe true
  }

  it should "limit summary to 1000 characters" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val posts = awaitFeed(feed).get

    posts.foreach { post =>
      post.summary.length should be <= 1000
    }
  }

  it should "return correct source type" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    feed.getSourceType() shouldBe "meltwater"
  }

  it should "return correct source" in {
    val sourcePath = getResourcePath("/meltwater/Examples/export-news-1.csv")
    val feed = new MeltwaterFeed(sourcePath)
    feed.getSource() shouldBe sourcePath
  }

  it should "handle malformed CSV gracefully" in {
    val feed = new MeltwaterFeed("/nonexistent.csv")
    val result = awaitFeed(feed)

    result shouldBe a[Failure[_]]
  }

  it should "handle UTF-16 LE encoding" in {
    // The actual Meltwater export files are UTF-16 LE encoded
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val result = awaitFeed(feed)

    result shouldBe a[Success[_]]
    val posts = result.get
    posts should not be empty

    // Verify that special characters are correctly decoded
    posts.foreach { post =>
      post.title should not include "\u0000"  // No null characters from bad encoding
    }
  }

  it should "create unique IDs for each post" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val posts = awaitFeed(feed).get

    // All posts should have unique IDs
    val ids = posts.map(_.id)
    ids.distinct.size shouldBe ids.size
  }

  it should "use URL as ID when available" in {
    val feed = new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv"))
    val posts = awaitFeed(feed).get

    val postsWithUrl = posts.filter(p => p.feedMetadata.get("URL").exists(!_.isBlank))

    postsWithUrl.foreach { post =>
      val url = post.feedMetadata("URL")
      post.id shouldBe url
    }
  }

  // ---------------------------------------------------------------------------
  // JSON (Meltwater Search API response shape) - primary format
  // ---------------------------------------------------------------------------

  "MeltwaterFeed (JSON)" should "parse a Search API JSON response from file" in {
    val feed = new MeltwaterFeed(jsonResource("/meltwater/search-mentions-1.json"))
    val result = awaitFeed(feed)

    result shouldBe a[Success[_]]
    val posts = result.get
    posts should have size 10

    posts.foreach { post =>
      post.id should not be empty
      post.typ shouldBe "meltwater"
      post.publishedDate should be > 0L
    }
  }

  it should "map JSON document fields into NewsPost metadata" in {
    val feed = new MeltwaterFeed(jsonResource("/meltwater/search-mentions-1.json"))
    val posts = awaitFeed(feed).get

    // The news article document carries the richest fields.
    val newsPost = posts.find(_.feedMetadata.get("Content_Type").contains("news article"))
    newsPost shouldBe defined

    val p = newsPost.get
    p.title should not be empty
    p.link should startWith("https://www.ainvest.com")
    p.feedMetadata.get("Source") shouldBe Some("AInvest")
    p.feedMetadata.get("Domain") shouldBe Some("ainvest.com")
    p.feedMetadata.get("Language") shouldBe Some("en")
    p.feedMetadata.get("Sentiment") shouldBe Some("negative")
    p.feedMetadata.get("Reach") shouldBe Some("176892")
    p.feedMetadata.get("Search_Id") shouldBe Some("28737363")
    p.feedMetadata.get("Search_Name") shouldBe Some("DLT Foundations - Adverse Signals")
    p.feedMetadata.get("Keywords") shouldBe Some("The Open Network, collapse")
  }

  it should "parse published_date from ISO-8601 to epoch millis" in {
    val feed = new MeltwaterFeed(jsonResource("/meltwater/search-mentions-1.json"))
    val posts = awaitFeed(feed).get

    posts.foreach { post =>
      post.publishedDate should be > 0L
      // All documents are from 2026-06.
      post.publishedDate should be >= 1780272000000L // 2026-06-01
      post.publishedDate should be < 1782864000000L  // 2026-07-01
    }
  }

  it should "detect json mode for plain and file:// paths" in {
    val path = jsonResource("/meltwater/search-mentions-1.json")
    new MeltwaterFeed(path).getSourceType() shouldBe "meltwater"
    awaitFeed(new MeltwaterFeed("file://" + path)).get should have size 10
  }

  it should "resolve apiKey and ids from a meltwater:// URI (?apiKey=)" in {
    val feed = new MeltwaterFeed("meltwater://100?apiKey=KEY")
    feed.getSourceType() shouldBe "meltwater"
    feed.getApiKey shouldBe "KEY"
    feed.getSearchIds shouldBe Seq("100")
  }

  it should "let an explicit constructor apiKey win over the URI/env" in {
    val feed = new MeltwaterFeed("meltwater://100", apiKey = "EXPLICIT")
    feed.getApiKey shouldBe "EXPLICIT"
    feed.getSearchIds shouldBe Seq("100")
  }

  it should "expose no search ids for csv/json sources" in {
    new MeltwaterFeed(csvResource("/meltwater/Examples/export-news-1.csv")).getSearchIds shouldBe empty
    new MeltwaterFeed(jsonResource("/meltwater/search-mentions-1.json")).getSearchIds shouldBe empty
  }
}
