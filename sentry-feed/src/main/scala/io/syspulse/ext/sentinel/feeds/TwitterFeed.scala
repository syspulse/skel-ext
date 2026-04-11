package io.syspulse.ext.sentinel.feeds

import com.typesafe.scalalogging.Logger
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

import io.syspulse.skel.twitter.Twit
import io.syspulse.skel.twitter.TwitterConnect

class TwitterFeed(source: String, max:Option[Long] = None, timeout0:Option[Long] = None) extends NewsFeed {  
  private val log = Logger(getClass.getName)

  override def toString = s"TwitterFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "twitter"

  val DEF_PAST = 1000 * 60 * 60 * 24 * 5 // 7 days
  val DEF_TIMEOUT = 30000 // 30 seconds

  // Cache to track seen tweets to avoid duplicates
  private var seenTweets = Set.empty[String]
  private val maxCacheSize = 1000

  // TwitterConnect knows how to parse the twitter:// URI
  private val client = new TwitterConnect(source, Some(DEF_PAST), max.map(m => if (m < 10) 10 else m))

  override def fetchFeed(timeout: Long = 0L)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    //implicit val ec0: ExecutionContext = ec
    val requestTimeout: FiniteDuration =
      FiniteDuration((if (timeout > 0) timeout else timeout0.getOrElse(DEF_TIMEOUT)), TimeUnit.MILLISECONDS)

    client
      .askAsync()(ec, requestTimeout)
      .map { tweets =>
        log.info(s"Fetched ${tweets.size} tweets from ${source}")

        val newTweets = tweets.filterNot(t => seenTweets.contains(t.id))
        seenTweets = (seenTweets ++ newTweets.map(_.id)).takeRight(maxCacheSize)

        newTweets.map(twitToNewsPost)
      }
  }

  private def extractFirstSentence(text: String,limit:Int = 80): String = {
    // Find the first sentence ending with "." or newline
    val sentenceEnd = text.indexWhere(c => c == '.' || c == '\n')
    val firstSentence = if (sentenceEnd >= 0) {
      text.substring(0, sentenceEnd + 1).trim
    } else {
      text.trim
    }
    
    // Limit to limit characters
    if (firstSentence.length > limit) {
      firstSentence.take(limit)
    } else {
      firstSentence
    }
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
      title = extractFirstSentence(twit.text),
      link = s"https://x.com/${twit.author_name}/status/${twit.id}",
      author = s"@${twit.author_name}",
      publishedDate = twit.created_at,
      summary = twit.text,
      source = source,
      typ = "twitter",
      categories = categories,
      images = twit.media.toList,
      feedMetadata = metadata
    )
  }
}
