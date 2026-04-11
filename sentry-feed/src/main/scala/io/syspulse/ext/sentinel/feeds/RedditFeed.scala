package io.syspulse.ext.sentinel.feeds

import scala.util.{Try, Success, Failure}
import scala.xml.XML
import scala.xml.Elem
import com.typesafe.scalalogging.Logger
import scala.concurrent.{ExecutionContext, Future}

import io.syspulse.skel.HTTP

class RedditFeed(source: String) extends NewsFeed {
  private val log = Logger(getClass.getName)

  override def toString = s"RedditFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "reddit"

  override def fetchFeed(timeout: Long)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    
    loadXml(source, timeout)(ec).map { xml =>
      // 2. Parse Atom 1.0 structure
      val entries = (xml \\ "entry")

      entries.map { entry =>
        val id = (entry \ "id").text.trim
        val title = (entry \ "title").text.trim
        val link = (entry \ "link" \ "@href").text.trim
        val author = (entry \ "author" \ "name").text.trim
        val publishedStr = (entry \ "published").text.trim
        val content = (entry \ "content").text.trim

        // Parse ISO-8601 date
        val publishedDate = DateParser.parseIso8601(publishedStr)

        // Extract subreddit from category
        val subreddit = (entry \ "category" \ "@term").text.trim
        val subredditLabel = (entry \ "category" \ "@label").text.trim

        // Categories for Reddit is the subreddit
        val categories =
          if (subredditLabel.nonEmpty) List(subredditLabel)
          else if (subreddit.nonEmpty) List(subreddit)
          else List.empty

        // Extract thumbnails if present (from media:thumbnail)
        // Note: Scala XML strips namespace prefixes, so we use just "thumbnail"
        val images = (entry \\ "thumbnail")
          .map(thumb => (thumb \ "@url").text.trim)
          .filter(_.nonEmpty)
          .toList

        val metadata = Map(
          "subreddit" -> subredditLabel
        ).filter(_._2.nonEmpty)

        NewsPost(
          id = id,
          title = title,
          link = link,
          author = if (author.nonEmpty) author else "Unknown",
          publishedDate = publishedDate,
          summary = stripHtml(content).take(1000),
          source = source,
          typ = "reddit",
          categories = categories,
          images = images,
          feedMetadata = metadata
        )
      }.toSeq
    }
  }

  private def loadXml(source: String, timeoutMs: Long)(ec: ExecutionContext): Future[Elem] = {
    if (source.startsWith("http://") || source.startsWith("https://")) {
      HTTP.get(source, timeoutMs).map(XML.loadString)(ec)
    } else {
      val path = if (source.startsWith("file://")) source.substring(7) else source
      Future(XML.loadFile(path))(ec)
    }
  }

  private def stripHtml(html: String): String = {
    // More aggressive HTML stripping for Reddit's HTML content
    html.replaceAll("<[^>]*>", "")
        .replaceAll("&nbsp;", " ")
        .replaceAll("&[^;]+;", "")
        .replaceAll("\\s+", " ")
        .trim
  }
}
