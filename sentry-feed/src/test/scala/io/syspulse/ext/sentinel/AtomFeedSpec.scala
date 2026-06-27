package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

import io.syspulse.ext.sentinel.feeds.{AtomFeed, NewsPost}
import io.hacken.ext.sentinel.Config

class AtomFeedSpec extends AnyFlatSpec with Matchers {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private implicit val config: Config = Config()

  private def awaitPosts(feed: AtomFeed, timeoutMs: Long = 0L): Seq[NewsPost] =
    Await.result(feed.fetchFeed(timeoutMs)(ec), (config.detectorTimeout + 1000L).millis)

  private def blockworksAtomPath: String = {
    val candidates = Seq(
      "./rss/blockworks.atom",              // when running from sentry-feed/
      "sentry-feed/rss/blockworks.atom"     // when running from repo root
    )
    candidates.find(p => new java.io.File(p).exists()).getOrElse(candidates.head)
  }

  "AtomFeed" should "parse Blockworks Atom feed from file" in {
    val feed = new AtomFeed(blockworksAtomPath)
    val posts = awaitPosts(feed)

    posts should not be empty

    val firstPost = posts.head
    firstPost.id should not be empty
    firstPost.title should not be empty
    firstPost.link should startWith("https://")
    firstPost.publishedDate should be > 0L
    firstPost.typ shouldBe "atom"
  }

  it should "return correct source type" in {
    val feed = new AtomFeed(blockworksAtomPath)
    feed.getSourceType() shouldBe "atom"
  }
}

