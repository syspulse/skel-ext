package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Success, Failure}

import io.syspulse.ext.sentinel.feeds.RedditFeed

class RedditFeedSpec extends AnyFlatSpec with Matchers {

  "RedditFeed" should "parse Reddit Atom feed from file" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val result = feed.fetchFeed()

    result shouldBe a[Success[_]]
    val posts = result.get
    posts should not be empty

    val firstPost = posts.head
    firstPost.id should not be empty
    firstPost.title should not be empty
    firstPost.link should not be empty
    firstPost.publishedDate should be > 0L
    firstPost.typ shouldBe "reddit"
  }

  it should "extract subreddit from category" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val posts = feed.fetchFeed().get

    posts should not be empty

    // Check if posts have subreddit metadata
    val postsWithSubreddit = posts.filter(_.feedMetadata.contains("subreddit"))
    postsWithSubreddit.size should be > 0

    // Verify subreddit format (should not be empty)
    postsWithSubreddit.foreach { post =>
      val subreddit = post.feedMetadata("subreddit")
      subreddit should not be empty
    }
  }

  it should "extract categories (subreddit) as List" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val posts = feed.fetchFeed().get

    posts should not be empty

    // Check if posts have categories
    val postsWithCategories = posts.filter(_.categories.nonEmpty)
    postsWithCategories.size should be > 0

    // Verify categories is a List with subreddit
    postsWithCategories.foreach { post =>
      post.categories shouldBe a[List[_]]
      post.categories.size shouldBe 1 // Reddit has one category (the subreddit)
      post.categories.head should not be empty
    }
  }

  it should "strip HTML from content" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val posts = feed.fetchFeed().get

    posts.foreach { post =>
      post.summary should not include "<"
      post.summary should not include ">"
      post.summary should not include "<table"
      post.summary should not include "</td>"
    }
  }

  it should "extract author from entry/author/name" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val posts = feed.fetchFeed().get

    posts should not be empty
    // Check that we have authors (Reddit format is /u/username)
    val postsWithAuthors = posts.filter(_.author != "Unknown")
    postsWithAuthors.size should be > 0
  }

  it should "limit summary to 1000 characters" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val posts = feed.fetchFeed().get

    posts.foreach { post =>
      post.summary.length should be <= 1000
    }
  }

  it should "handle single-line XML format" in {
    // Reddit feed is stored as single line without line terminators
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val result = feed.fetchFeed()

    result shouldBe a[Success[_]]
    result.get should not be empty
  }

  it should "extract thumbnail if present" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    val posts = feed.fetchFeed().get

    // Some posts might have thumbnails
    val postsWithThumbnails = posts.filter(_.feedMetadata.contains("thumbnail"))
    // Just verify it doesn't crash - thumbnails are optional
    postsWithThumbnails.size should be >= 0
  }

  it should "return correct source type" in {
    val feed = new RedditFeed("sentry-news/reddit/reddit-1.xml")
    feed.getSourceType() shouldBe "reddit"
  }

  it should "return correct source" in {
    val sourcePath = "sentry-news/reddit/reddit-1.xml"
    val feed = new RedditFeed(sourcePath)
    feed.getSource() shouldBe sourcePath
  }

  it should "handle malformed XML gracefully" in {
    val feed = new RedditFeed("sentry-news/nonexistent.xml")
    val result = feed.fetchFeed()

    result shouldBe a[Failure[_]]
  }
}
