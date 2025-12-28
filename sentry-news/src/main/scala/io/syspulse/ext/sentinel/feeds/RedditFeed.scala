package io.syspulse.ext.sentinel.feeds

import scala.util.{Try, Success, Failure}
import scala.xml.XML
import scala.xml.Elem
import com.typesafe.scalalogging.Logger

class RedditFeed(source: String) extends NewsFeed {
  private val log = Logger(getClass.getName)

  override def toString = s"RedditFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "reddit"

  override def fetchFeed(): Try[Seq[NewsPost]] = Try {
    // 1. Load XML
    val xml = loadXml(source)

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
      val categories = if (subredditLabel.nonEmpty) List(subredditLabel) else if (subreddit.nonEmpty) List(subreddit) else List.empty

      // Extract thumbnail if present
      val thumbnail = (entry \ "{http://search.yahoo.com/mrss/}thumbnail" \ "@url").text.trim

      val metadata = Map(
        "subreddit" -> subredditLabel,
        "thumbnail" -> thumbnail
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
        feedMetadata = metadata
      )
    }.toSeq
  }

  private def loadXml(source: String): Elem = {
    if (source.startsWith("http://") || source.startsWith("https://")) {
      val response = requests.get(source, readTimeout = 30000, connectTimeout = 10000)
      if (response.statusCode == 200) {
        XML.loadString(response.text())
      } else {
        throw new Exception(s"HTTP ${response.statusCode}: ${response.text()}")
      }
    } else {
      val path = if (source.startsWith("file://")) source.substring(7) else source
      XML.loadFile(path)
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
