package io.syspulse.ext.sentinel.feeds

import scala.util.{Try, Success, Failure}
import scala.xml.XML
import scala.xml.Elem
import com.typesafe.scalalogging.Logger
import scala.concurrent.{ExecutionContext, Future}

import io.syspulse.skel.HTTP

class RssFeed(source: String) extends NewsFeed {
  private val log = Logger(getClass.getName)

  override def toString = s"RssFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "rss"

  override def fetchFeed(timeout: Long)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    loadXml(source, timeout)(ec).map { xml =>
      log.debug(s"RSS: ${source}: '${xml}'")
      // 2. Parse RSS 2.0 structure
      val items = (xml \\ "item")

      log.info(s"RSS: ${source}: items=${items.size}")

      items.map { item =>

        val guid = (item \ "guid").text.trim
        val title = (item \ "title").text.trim
        val link = (item \ "link").text.trim
        // Extract author from dc:creator field (try multiple namespace access methods)
        val ns = "http://purl.org/dc/elements/1.1/"
        val author = {
          val direct = (item \ s"{$ns}creator").text.trim
          val deep = (item \\ s"{$ns}creator").headOption.map(_.text.trim).getOrElse("")
          val unprefixed = (item \ "creator").text.trim // Fallback if namespace prefix is stripped
          if (direct.nonEmpty) direct else if (deep.nonEmpty) deep else unprefixed
        }.trim
        val pubDateStr = (item \ "pubDate").text.trim
        val description = (item \ "description").text.trim

        // Parse RFC-822 date format
        val publishedDate = DateParser.parseRfc822(pubDateStr)

        // Extract categories as List
        val categories = (item \ "category").map(_.text.trim).filter(_.nonEmpty).toList

        // Extract media URLs if present (from media:content with medium="image")
        val mediaNamespace = "http://search.yahoo.com/mrss/"
        val mediaUrls = (item \ s"{$mediaNamespace}content")
          .filter(content => (content \ "@medium").text == "image")
          .map(content => (content \ "@url").text.trim)
          .filter(_.nonEmpty)
          .toList

        // Fallback to enclosure if no media:content found
        val enclosureUrls =
          if (mediaUrls.isEmpty) {
            (item \ "enclosure")
              .filter(enc => (enc \ "@type").text.startsWith("image/"))
              .map(enc => (enc \ "@url").text.trim)
              .filter(_.nonEmpty)
              .toList
          } else {
            List.empty[String]
          }

        val images = if (mediaUrls.nonEmpty) mediaUrls else enclosureUrls

        val metadata = Map.empty[String, String]

        val stableIdSource = {
          def hostFromUrl(u: String): String =
            Try(new java.net.URI(u)).toOption.flatMap(uri => Option(uri.getHost)).getOrElse("")

          def isHostLike(s: String): Boolean = {
            val t = s.trim
            t.nonEmpty &&
            !t.contains("/") &&
            !t.contains(":") &&
            t.contains(".")
          }

          if (guid.nonEmpty) {
            // Some feeds incorrectly set guid to just host (e.g. "cryptoslate.com"),
            // which is not unique per entry. In that case prefer link.
            val linkHost = if (link.startsWith("http://") || link.startsWith("https://")) hostFromUrl(link) else ""
            val guidLooksBad =
              isHostLike(guid) && linkHost.nonEmpty &&
                (guid == linkHost || guid == linkHost.stripPrefix("www."))

            if (guidLooksBad && link.nonEmpty) link else guid
          } else if (link.nonEmpty) {
            link
          } else {
            title
          }
        }
        val id = FeedId.stableId(stableIdSource)

        NewsPost(
          id = id,
          title = title,
          link = link,
          author = if (author.nonEmpty) author else "Unknown",
          publishedDate = publishedDate,
          summary = stripHtml(description).take(1000), // Strip CDATA/HTML, limit length
          source = source,
          typ = "rss",
          categories = categories,
          images = images,
          feedMetadata = metadata
        )
      }.toSeq
    }
  }

  private def loadXml(source: String, timeoutMs: Long)(ec: ExecutionContext): Future[Elem] = {
    if (source.startsWith("http://") || source.startsWith("https://")) {
      HTTP.get(source, timeout = timeoutMs).map(XML.loadString)(ec)
    } else {
      val path = if (source.startsWith("file://")) source.substring(7) else source
      Future(XML.loadFile(path))(ec)
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
