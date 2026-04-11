package io.syspulse.ext.sentinel.feeds

import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.Logger
import scala.concurrent.{ExecutionContext, Future}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.io.{ByteArrayInputStream, InputStreamReader, Reader}

import io.syspulse.skel.HTTP
import io.syspulse.skel.util.Util
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

class MeltwaterFeed(source: String) extends NewsFeed {
  private val log = Logger(getClass.getName)

  override def toString = s"MeltwaterFeed($source)"

  override def getSource(): String = source
  override def getSourceType(): String = "meltwater"

  override def fetchFeed(timeout: Long)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    loadCsv(source, timeout)(ec).map { rows =>
      rows.flatMap { row =>
        parseMeltwaterRow(row)
      }
    }
  }

  private def loadCsv(source: String, timeoutMs: Long)(ec: ExecutionContext): Future[Seq[Map[String, String]]] = {
    if (source.startsWith("http://") || source.startsWith("https://")) {
      // Download from HTTP
      HTTP.get(source, timeoutMs).map { content =>
        val bytes = content.getBytes(StandardCharsets.UTF_8)
        parseCsvBytes(bytes)
      }(ec)
    } else {
      // Load from file
      val path = if (source.startsWith("file://")) source.substring(7) else source
      Future {
        parseCsvBytes(Files.readAllBytes(Paths.get(path)))
      }(ec)
    }
  }

  private object MeltwaterCsvFormat extends DefaultCSVFormat {
    override val delimiter: Char = ','
    override val quoteChar: Char = '"'
    override val escapeChar: Char = '"'
    override val lineTerminator: String = "\n"
  }

  private def bytesToReader(bytes: Array[Byte]): Reader = {
    // Detect UTF-16 LE BOM (0xFF 0xFE), otherwise fall back to UTF-8.
    val charset: Charset =
      if (bytes.length >= 2 && bytes(0) == 0xFF.toByte && bytes(1) == 0xFE.toByte) Charset.forName("UTF-16LE")
      else StandardCharsets.UTF_8

    new InputStreamReader(new ByteArrayInputStream(bytes), charset)
  }

  /** CSV column names become NewsPost feedMetadata keys with spaces replaced by `_`. */
  private def normalizeMetaKey(k: String): String = k.replace(' ', '_')

  private def parseCsvBytes(bytes: Array[Byte]): Seq[Map[String, String]] = {
    val reader = bytesToReader(bytes)
    val csv = CSVReader.open(reader)(MeltwaterCsvFormat)
    try {
      val all = csv.allWithHeaders()
      all.map(_.view.map { case (k, v) => (normalizeMetaKey(k), v.trim) }.toMap)
    } finally {
      csv.close()
      reader.close()
    }
  }

  private def parseMeltwaterRow(row: Map[String, String]): Option[NewsPost] = {
    try {
      val dateStr = row.getOrElse("Date", "")
      val headline = row.getOrElse("Headline", "")
      val url = row.getOrElse("URL", "")
      val openingText = row.getOrElse("Opening_Text", "")
      val source = row.getOrElse("Source", "")
      val influencer = row.getOrElse("Influencer", "")
      val country = row.getOrElse("Country", "")
      val language = row.getOrElse("Language", "")

      // Parse date: "2021-07-14 05:47:10"
      val publishedDate = parseMeltwaterDate(dateStr)

      def defaultId(source: String, dateStr: String, headline: String): String = {
        s"${source}_${dateStr}_${headline.hashCode}"
      }

      // Create unique ID from URL and date
      val id = source.toLowerCase() match {
        case "twitter" => row.getOrElse("Tweet_id", defaultId(source, dateStr, headline))
        case "facebook" => row.getOrElse("URL", defaultId(source, dateStr, headline))
        case "reddit" => row.getOrElse("URL", defaultId(source, dateStr, headline))
        case _ if !url.isBlank => url
        case _ => defaultId(source, dateStr, headline)
      }

      // Collect all metadata (all fields except Date)
      val metadata = row.filter { case (k, _) => k != "Date" }

      Some(NewsPost(
        id = id,
        title = headline,
        link = url,
        author = influencer,
        publishedDate = publishedDate,
        summary = openingText.take(1000),
        source = this.source,
        typ = "meltwater",
        categories = List.empty,
        images = List.empty,
        feedMetadata = metadata
      ))
    } catch {
      case e: Exception =>
        log.warn(s"Failed to parse Meltwater row: ${e.getMessage}")
        None
    }
  }

  private def parseMeltwaterDate(dateStr: String): Long = {
    try {
      // Format: "2021-07-14 05:47:10"
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
      val localDateTime = LocalDateTime.parse(dateStr, formatter)
      // Convert to epoch millis (assuming UTC)
      localDateTime.atZone(java.time.ZoneOffset.UTC).toInstant.toEpochMilli
    } catch {
      case e: Exception =>
        log.warn(s"Failed to parse date '$dateStr': ${e.getMessage}")
        System.currentTimeMillis()
    }
  }
}
