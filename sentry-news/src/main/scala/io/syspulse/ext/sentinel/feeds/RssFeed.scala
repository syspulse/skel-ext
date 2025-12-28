package io.syspulse.ext.sentinel.feeds

import scala.util.{Try, Success, Failure}
import scala.xml.XML
import scala.xml.Elem
import com.typesafe.scalalogging.Logger

class RssFeed(source: String) extends NewsFeed {
  private val log = Logger(getClass.getName)

  override def toString = s"RssFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "rss"

  override def fetchFeed(): Try[Seq[NewsPost]] = Try {
    // 1. Load XML
    val xml = loadXml(source)

    // 2. Parse RSS 2.0 structure
    val items = (xml \\ "item")

    items.map { item =>
      val guid = (item \ "guid").text.trim
      val title = (item \ "title").text.trim
      val link = (item \ "link").text.trim
      // Extract author from dc:creator field (try multiple namespace access methods)
      val ns = "http://purl.org/dc/elements/1.1/"
      val author = {
        val direct = (item \ s"{$ns}creator").text.trim
        val deep = (item \\ s"{$ns}creator").headOption.map(_.text.trim).getOrElse("")
        val unprefixed = (item \ "creator").text.trim  // Fallback if namespace prefix is stripped
        if (direct.nonEmpty) direct else if (deep.nonEmpty) deep else unprefixed
      }.trim
      val pubDateStr = (item \ "pubDate").text.trim
      val description = (item \ "description").text.trim

      // Parse RFC-822 date format
      val publishedDate = DateParser.parseRfc822(pubDateStr)

      // Extract categories as List
      val categories = (item \ "category").map(_.text.trim).filter(_.nonEmpty).toList

      // Extract media URL if present
      val mediaUrl = (item \ "{http://search.yahoo.com/mrss/}content" \ "@url").text.trim

      val metadata = Map(
        "media_url" -> mediaUrl
      ).filter(_._2.nonEmpty)

      NewsPost(
        id = if (guid.nonEmpty) guid else link,  // Fallback to link if no GUID
        title = title,
        link = link,
        author = if (author.nonEmpty) author else "Unknown",
        publishedDate = publishedDate,
        summary = stripHtml(description).take(1000),  // Strip CDATA/HTML, limit length
        source = source,
        typ = "rss",
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

  private def stripHtml(text: String): String = {
    // Remove CDATA wrapper
    val noCdata = text.replaceAll("<!\\[CDATA\\[", "").replaceAll("\\]\\]>", "")
    // Strip HTML tags
    noCdata.replaceAll("<[^>]*>", "")
           .replaceAll("&nbsp;", " ")
           .replaceAll("&[^;]+;", "")
           .trim
  }
}
