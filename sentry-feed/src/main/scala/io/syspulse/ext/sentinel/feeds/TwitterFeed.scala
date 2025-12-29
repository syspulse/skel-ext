package io.syspulse.ext.sentinel.feeds

import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.Logger
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

import io.syspulse.skel.twitter.TwitterConnect
import io.syspulse.skel.twitter.Twit

class TwitterFeed(source: String, max:Option[Long] = None, timeout0:Option[Long] = None) extends NewsFeed {  
  private val log = Logger(getClass.getName)

  override def toString = s"TwitterFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "twitter"

  val DEF_PAST = 1000 * 60 * 60 * 24 * 5 // 7 days
  val DEF_TIMEOUT = 30000 // 30 seconds

  // Execution context and timeout for Twitter API calls
  private implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  private implicit val timeout: FiniteDuration = FiniteDuration(timeout0.getOrElse(DEF_TIMEOUT), TimeUnit.MILLISECONDS)

  // Cache to track seen tweets to avoid duplicates
  private var seenTweets = Set.empty[String]
  private val maxCacheSize = 1000

  // Create TwitterConnect client once during construction and reuse
  // TwitterConnect knows how to parse the twitter:// URI
  private val client = new TwitterConnect(source,Some(DEF_PAST),max.map(m => if(m < 10) 10 else m))

  override def fetchFeed(): Try[Seq[NewsPost]] = Try {
    // Fetch tweets using ask() method
    val tweets = client.ask()

    log.info(s"Fetched ${tweets.size} tweets from ${source}")

    // Filter out already seen tweets
    val newTweets = tweets.filterNot(t => seenTweets.contains(t.id))

    // Update seen tweets cache (keep last maxCacheSize tweets)
    seenTweets = (seenTweets ++ newTweets.map(_.id)).takeRight(maxCacheSize)

    // Convert Twit objects to NewsPost objects
    newTweets.map(twitToNewsPost)
  }

  private def twitToNewsPost(twit: Twit): NewsPost = {
    // Extract categories from media presence
    val categories = if (twit.media.nonEmpty) List("media") else List.empty

    // Build metadata
    val metadata = Map(
      "author_id" -> twit.author_id,
      "link" -> s"https://x.com/${twit.author_name}/status/${twit.id}"
    )

    NewsPost(
      id = twit.id,
      title = s"@${twit.author_name}: ${twit.text.take(100)}${if (twit.text.length > 100) "..." else ""}",
      link = s"https://x.com/${twit.author_name}/status/${twit.id}",
      author = s"@${twit.author_name}",
      publishedDate = twit.created_at,
      summary = twit.text.take(1000),
      source = source,
      typ = "twitter",
      categories = categories,
      images = twit.media.toList,
      feedMetadata = metadata
    )
  }
}
