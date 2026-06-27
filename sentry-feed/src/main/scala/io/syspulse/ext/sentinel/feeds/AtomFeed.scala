package io.syspulse.ext.sentinel.feeds

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{Elem, XML}
import com.typesafe.scalalogging.Logger

import io.syspulse.skel.HTTP

class AtomFeed(source: String) extends NewsFeed {
  private val log = Logger(getClass.getName)

  override def toString: String = s"AtomFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "atom"

  override def fetchFeed(timeout: Long)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    loadXml(source, timeout)(ec).map { xml =>
      log.debug(s"Atom: ${source}: '${xml}'")
      val entries = (xml \\ "entry")
      log.info(s"Atom: ${source}: entries=${entries.size}")

      entries.map { entry =>
        val id0 = (entry \ "id").text.trim
        val title = (entry \ "title").text.trim

        // Prefer rel="alternate" if present, else first link@href
        val links = (entry \ "link")
        val link = links
          .find(n => (n \ "@rel").text.trim == "alternate" && (n \ "@href").text.trim.nonEmpty)
          .orElse(links.find(n => (n \ "@href").text.trim.nonEmpty))
          .map(n => (n \ "@href").text.trim)
          .getOrElse("")

        val author = (entry \ "author" \ "name").text.trim

        val publishedStr = (entry \ "published").text.trim
        val updatedStr = (entry \ "updated").text.trim
        val publishedDate =
          if (publishedStr.nonEmpty) DateParser.parseIso8601(publishedStr)
          else if (updatedStr.nonEmpty) DateParser.parseIso8601(updatedStr)
          else System.currentTimeMillis()

        val summary = {
          val s = (entry \ "summary").text.trim
          if (s.nonEmpty) s else (entry \ "content").text.trim
        }

        val categories = (entry \ "category")
          .map { c =>
            val term = (c \ "@term").text.trim
            if (term.nonEmpty) term else c.text.trim
          }
          .filter(_.nonEmpty)
          .toList

        val stableIdSource =
          if (id0.nonEmpty) id0 else if (link.nonEmpty) link else title
        val id = FeedId.stableId(stableIdSource)

        NewsPost(
          id = id,
          title = title,
          link = link,
          author = if (author.nonEmpty) author else "Unknown",
          publishedDate = publishedDate,
          summary = stripHtml(summary).take(1000),
          source = source,
          typ = "atom",
          categories = categories,
          images = List.empty,
          feedMetadata = Map.empty[String, String]
        )
      }.toSeq
    }
  }

  private def loadXml(source: String, timeoutMs: Long)(ec: ExecutionContext): Future[Elem] = {
    if (source.startsWith("http://") || source.startsWith("https://")) {
      HTTP.get(source, timeout = timeoutMs).map(XML.loadString)(ec)
    } else {
      val path =
        if (source.startsWith("atom://")) source.substring(7)
        else if (source.startsWith("file://")) source.substring(7)
        else source
      Future(XML.loadFile(path))(ec)
    }
  }

  private def stripHtml(text: String): String = {
    val noCdata = text.replaceAll("<!\\[CDATA\\[", "").replaceAll("\\]\\]>", "")
    noCdata
      .replaceAll("<[^>]*>", "")
      .replaceAll("&nbsp;", " ")
      .replaceAll("&[^;]+;", "")
      .trim
  }
}

