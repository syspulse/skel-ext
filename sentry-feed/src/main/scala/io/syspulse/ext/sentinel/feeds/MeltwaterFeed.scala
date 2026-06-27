package io.syspulse.ext.sentinel.feeds

import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.Logger
import scala.concurrent.{ExecutionContext, Future}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.io.{ByteArrayInputStream, InputStreamReader, Reader}

import spray.json._

import io.syspulse.skel.HTTP
import io.syspulse.skel.util.Util
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

/** Query parameters for the Meltwater Search API (POST /v3/search/{id}).
  *
  * The window is a rolling window: start = now - windowMs, end = now.
  * The endpoint is paged and capped (page max 10, page_size max 100).
  */
case class MeltwaterQuery(
  windowMs: Long = MeltwaterFeed.DEF_WINDOW_MS,
  pageFrom: Int = MeltwaterFeed.DEF_PAGE_FROM,
  pageSize: Int = MeltwaterFeed.DEF_PAGE_SIZE,
  sortBy: String = MeltwaterFeed.DEF_SORT_BY,
  sortOrder: String = MeltwaterFeed.DEF_SORT_ORDER,
  tz: String = MeltwaterFeed.DEF_TZ,
  template: String = MeltwaterFeed.DEF_TEMPLATE
)

object MeltwaterFeed {
  val SRC_TYPE = "meltwater"

  // API base URL is internal to the feed and is intentionally NOT exposed as a detector config option.
  val DEF_API_URL = "https://api.meltwater.com/v3"

  val DEF_WINDOW_MS = 24L * 60 * 60 * 1000 // 24h rolling window
  val DEF_PAGE_FROM = 1
  val DEF_PAGE_SIZE = 10 // endpoint default
  val DEF_SORT_BY = "date"
  val DEF_SORT_ORDER = "desc"
  val DEF_TZ = "UTC"
  val DEF_TEMPLATE = "api.json"

  val DEF_TEXT_LIMIT = 1000

  val PREFIX_API = "meltwater://"
  val PREFIX_CSV = "csv://"
  val PREFIX_FILE = "file://"

  // Meltwater Search API timestamp format (naive local time, tz passed separately): "2026-06-01T00:00:00"
  private val API_TS_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")

  /** Parse a rolling-window string like "24h", "7d", "90m", "3600s" into milliseconds.
    * A bare number is treated as milliseconds. Blank/invalid falls back to the default.
    */
  def parseWindow(s: String, default: Long = DEF_WINDOW_MS): Long = {
    val v = Option(s).getOrElse("").trim.toLowerCase
    if (v.isEmpty) return default
    Try {
      val (numPart, unit) =
        if (v.last.isDigit) (v, "ms") else (v.dropRight(1), v.last.toString)
      val n = numPart.toLong
      unit match {
        case "s" => n * 1000L
        case "m" => n * 60L * 1000L
        case "h" => n * 60L * 60L * 1000L
        case "d" => n * 24L * 60L * 60L * 1000L
        case _   => n // ms
      }
    }.getOrElse(default)
  }

  /** Build a canonical X/Twitter status URL. */
  def twitterStatusUrl(tweetId: String, authorHandle: String = ""): String = {
    val tweet = Option(tweetId).getOrElse("").trim
    val handle = Option(authorHandle).getOrElse("").trim.stripPrefix("@")
    if (handle.nonEmpty) s"https://x.com/$handle/status/$tweet"
    else s"https://x.com/i/web/status/$tweet"
  }

  /** Extract @handle from an x.com / twitter.com profile URL. */
  def twitterHandleFromProfileUrl(profileUrl: String): String = {
    val u = Option(profileUrl).getOrElse("").trim
    if (u.isEmpty) return ""
    """(?:https?://)?(?:www\.)?(?:x\.com|twitter\.com)/(@?[^/?#]+)""".r
      .findFirstMatchIn(u)
      .flatMap(m => Option(m.group(1)).map(_.stripPrefix("@")))
      .filter(h => h.nonEmpty && h != "i" && h != "status" && h != "intent")
      .getOrElse("")
  }

  /** Resolve stable id and link (ref) from Meltwater fields.
    * Shared by CSV export parsing (`parseMeltwaterRow`) and Search API JSON (`documentToPost`).
    *
    * id: twitter -> tweet id; facebook/reddit -> URL; other with URL -> URL; else hash fallback.
    * url: twitter without URL -> x.com status URL; otherwise the provided URL.
    */
  def resolveIdentity(
    source: String,
    url0: String,
    tweetId: String,
    dateStr: String,
    headline: String,
    authorHandle: String = ""
  ): (String, String) = {
    val src = Option(source).getOrElse("").trim.toLowerCase
    val urlRaw = Option(url0).getOrElse("").trim
    val tweet = Option(tweetId).getOrElse("").trim
    val date = Option(dateStr).getOrElse("").trim
    val title = Option(headline).getOrElse("")

    def defaultId: String = s"${src}_${date}_${title.hashCode}"

    val url =
      if (urlRaw.isEmpty && src == "twitter" && tweet.nonEmpty) twitterStatusUrl(tweet, authorHandle)
      else urlRaw

    val id = src match {
      case "twitter" =>
        if (tweet.nonEmpty) tweet else defaultId
      case "facebook" | "reddit" =>
        if (url.nonEmpty) url else defaultId
      case _ if url.nonEmpty => url
      case _ => defaultId
    }

    (id, url)
  }
}

class MeltwaterFeed(
  source: String,
  apiKey: String = "",
  query: MeltwaterQuery = MeltwaterQuery()
) extends NewsFeed {
  import MeltwaterFeed._

  // API base URL is a constant of the feed, never a configurable/exposed option.
  private val apiUrl: String = DEF_API_URL

  private val log = Logger(getClass.getName)

  // Resolve the feed mode from the source prefix.
  //   meltwater://id,id?apiKey=KEY -> query the Search API (JSON). Empty ids -> list all searches, then query each.
  //   csv://path                   -> CSV payload (local file or http), same semantics as file://.
  //   file://, http(s)://, plain path -> JSON payload (a saved Search API response). JSON is the primary format.
  private val (mode, locator): (String, String) =
    if (source.startsWith(PREFIX_API)) ("api", source.substring(PREFIX_API.length))
    else if (source.startsWith(PREFIX_CSV)) ("csv", source.substring(PREFIX_CSV.length))
    else ("json", source)

  // meltwater:// is parsed via MeltwaterURI: search ids come from the path, apiKey ONLY from ?apiKey=.
  private lazy val mwUri: MeltwaterURI = MeltwaterURI(source)

  // Effective API key: an explicit constructor key wins; otherwise the URI's ?apiKey= / MELTWATER_API_KEY env.
  private lazy val effectiveApiKey: String =
    if (apiKey.nonEmpty) apiKey else mwUri.apiKey

  /** Resolved API key used for Search API calls (for inspection/testing). */
  def getApiKey: String = effectiveApiKey

  /** Search ids parsed from a meltwater:// URI (empty for csv/json sources or `meltwater://` with no ids). */
  def getSearchIds: Seq[String] = if (mode == "api") mwUri.ids else Seq.empty

  override def toString = s"MeltwaterFeed($source, mode=$mode)"

  override def getSource(): String = source
  override def getSourceType(): String = SRC_TYPE

  override def fetchFeed(timeout: Long)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    mode match {
      case "api"  => fetchApi(timeout)(ec)
      case "csv"  => loadBytes(locator, timeout)(ec).map(parseCsvBytes).map(_.flatMap(parseMeltwaterRow))(ec)
      case "json" => loadBytes(locator, timeout)(ec).map(b => parseMeltwaterJson(new String(b, StandardCharsets.UTF_8), None))(ec)
      case _      => Future.failed(new IllegalStateException(s"unknown Feed mode: '$mode'"))
    }
  }

  // ---------------------------------------------------------------------------
  // API (Meltwater Listening/Search API)
  // ---------------------------------------------------------------------------

  private def apiHeaders: Seq[(String, String)] = Seq(
    "apikey" -> effectiveApiKey,
    "Accept" -> "application/json",
    "Content-Type" -> "application/json"
  )

  private def fetchApi(timeout: Long)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    if (effectiveApiKey.isBlank) {
      return Future.failed(new IllegalArgumentException(
        s"MeltwaterFeed($source): apiKey required for API queries (pass ?apiKey= or set MELTWATER_API_KEY)"))
    }

    val explicitIds = mwUri.ids

    val idsF: Future[Seq[String]] =
      if (explicitIds.nonEmpty) Future.successful(explicitIds)
      else listSearchIds(timeout)(ec) // meltwater:// with no ids -> discover all searches

    idsF.flatMap { ids =>
      log.info(s"MeltwaterFeed($source): querying ${ids.size} search(es): ${ids.mkString(",")}")
      Future.sequence(ids.map(id => querySearch(id, timeout)(ec))).map(_.flatten)(ec)
    }(ec)
  }

  /** GET /v3/searches -> list of available search ids. */
  private def listSearchIds(timeout: Long)(implicit ec: ExecutionContext): Future[Seq[String]] = {
    val url = s"$apiUrl/searches"
    HTTP.get(url, headers = apiHeaders, timeout = timeout).map { content =>
      val searches = field(content.parseJson, "searches") match {
        case JsArray(els) => els.toList
        case _            => Nil
      }
      val ids = searches.flatMap(s => jstr(field(s, "id")))
      log.info(s"MeltwaterFeed($source): discovered ${ids.size} searches")
      ids
    }(ec)
  }

  /** POST /v3/search/{id} -> matching documents for one search. */
  private def querySearch(searchId: String, timeout: Long)(implicit ec: ExecutionContext): Future[Seq[NewsPost]] = {
    val url = s"$apiUrl/search/$searchId"
    val body = buildSearchBody()
    HTTP.post(url, body = Some(body), headers = apiHeaders, timeout = timeout)
      .map(content => parseMeltwaterJson(content, Some(searchId)))(ec)
      .recover {
        case e: Exception =>
          log.warn(s"MeltwaterFeed($source): search $searchId failed: ${e.getMessage}")
          Seq.empty[NewsPost]
      }(ec)
  }

  private def buildSearchBody(): String = {
    val now = Instant.now()
    val start = LocalDateTime.ofInstant(now.minusMillis(query.windowMs), ZoneOffset.UTC).format(API_TS_FORMAT)
    val end = LocalDateTime.ofInstant(now, ZoneOffset.UTC).format(API_TS_FORMAT)

    JsObject(
      "start" -> JsString(start),
      "end" -> JsString(end),
      "page" -> JsNumber(query.pageFrom),
      "page_size" -> JsNumber(query.pageSize),
      "sort_by" -> JsString(query.sortBy),
      "sort_order" -> JsString(query.sortOrder),
      "tz" -> JsString(query.tz),
      "template" -> JsObject("name" -> JsString(query.template))
    ).compactPrint
  }

  // ---------------------------------------------------------------------------
  // Loading bytes (file or http)
  // ---------------------------------------------------------------------------

  private def loadBytes(loc: String, timeoutMs: Long)(implicit ec: ExecutionContext): Future[Array[Byte]] = {
    if (loc.startsWith("http://") || loc.startsWith("https://")) {
      HTTP.get(loc, timeout = timeoutMs).map(_.getBytes(StandardCharsets.UTF_8))(ec)
    } else {
      val path = if (loc.startsWith(PREFIX_FILE)) loc.substring(PREFIX_FILE.length) else loc
      Future {
        Files.readAllBytes(Paths.get(path))
      }(ec)
    }
  }

  // ---------------------------------------------------------------------------
  // JSON parsing (Meltwater Search API response shape)
  // ---------------------------------------------------------------------------

  /** spray helpers: tolerant field access over the deeply-nested, null-heavy response. */
  private def field(v: JsValue, k: String): JsValue = v match {
    case JsObject(fs) => fs.getOrElse(k, JsNull)
    case _            => JsNull
  }
  private def path(v: JsValue, keys: String*): JsValue = keys.foldLeft(v)(field)
  private def jstr(v: JsValue): Option[String] = v match {
    case JsString(s)  => Some(s)
    case JsNumber(n)  => Some(n.bigDecimal.stripTrailingZeros.toPlainString)
    case JsBoolean(b) => Some(b.toString)
    case _            => None
  }
  private def jsArrStrings(v: JsValue): List[String] = v match {
    case JsArray(els) => els.toList.flatMap(jstr)
    case _            => Nil
  }
  private def jsArrField(v: JsValue, k: String): List[String] = v match {
    case JsArray(els) => els.toList.flatMap(e => jstr(field(e, k)))
    case _            => Nil
  }

  private def parseMeltwaterJson(content: String, queriedSearchId: Option[String]): Seq[NewsPost] = {
    Try(content.parseJson) match {
      case Failure(e) =>
        log.warn(s"MeltwaterFeed($source): failed to parse JSON: ${e.getMessage}")
        Seq.empty
      case Success(root) =>
        val documents: Seq[JsValue] = root match {
          case JsArray(els) => els.toList // bare array of documents
          case obj =>
            // full response { ..., result: { documents: [...] } } or { documents: [...] }
            field(obj, "result") match {
              case JsObject(_) => path(obj, "result", "documents") match {
                case JsArray(els) => els.toList
                case _            => Nil
              }
              case _ => field(obj, "documents") match {
                case JsArray(els) => els.toList
                case _            => Nil
              }
            }
        }
        documents.flatMap(doc => documentToPost(doc, queriedSearchId))
    }
  }

  private def documentToPost(doc: JsValue, queriedSearchId: Option[String]): Option[NewsPost] = {
    try {
      val externalId = jstr(field(doc, "external_id"))
      val url0 = jstr(field(doc, "url"))
        .orElse(jstr(path(doc, "source", "url")))
        .orElse(jstr(path(doc, "thread", "url")))
        .getOrElse("")

      val matchedInputs = path(doc, "matched", "inputs")
      val sourceName = jstr(path(doc, "source", "name")).getOrElse("")
      val title = jstr(path(doc, "content", "title"))
        .orElse(jstr(path(doc, "headline")))
        .orElse(jstr(path(doc, "matched", "title")))
        .orElse(jsArrField(matchedInputs, "name").headOption)
        .getOrElse("")

      val openingText = jstr(path(doc, "content", "opening_text"))
        .orElse(jstr(path(doc, "content", "body")))
        .orElse(jstr(path(doc, "matched", "hit_sentence")))
        .getOrElse("")

      val author = jstr(path(doc, "author", "name"))
        .orElse(jstr(path(doc, "author", "handle")))
        .orElse(jstr(path(doc, "author", "external_id")))
        .getOrElse("")

      val publishedDateStr = jstr(field(doc, "published_date")).getOrElse("")
      val publishedDate = parseIsoDate(publishedDateStr)

      val tweetId =
        if (sourceName.equalsIgnoreCase("twitter")) externalId.getOrElse("") else ""
      val authorHandle = {
        val handle = jstr(path(doc, "author", "handle")).getOrElse("").trim
        if (handle.nonEmpty) handle
        else twitterHandleFromProfileUrl(jstr(path(doc, "author", "profile_url")).getOrElse(""))
      }
      val (id, url) = MeltwaterFeed.resolveIdentity(
        sourceName, url0, tweetId, publishedDateStr, title, authorHandle)

      // Metadata keys mirror the CSV column names so DetectorMeltwater filters work for both formats.
      val meta = scala.collection.mutable.LinkedHashMap.empty[String, String]
      def add(k: String, v: Option[String]): Unit =
        v.map(_.trim).filter(_.nonEmpty).foreach(meta(k) = _)

      add("mw_headline", Some(title))
      add("mw_url", Some(url))
      add("mw_source", jstr(path(doc, "source", "name")))
      add("mw_domain", jstr(path(doc, "source", "domain")))
      add("mw_influencer", Some(author))
      add("mw_country", jstr(path(doc, "location", "country_code")))
      add("mw_language", jstr(path(doc, "enrichments", "language_code")))
      add("mw_reach", jstr(path(doc, "source", "metrics", "reach")))
      add("mw_twitter_social_echo", jstr(path(doc, "metrics", "social_echo", "x")))
      add("mw_facebook_social_echo", jstr(path(doc, "metrics", "social_echo", "facebook")))
      add("mw_reddit_social_echo", jstr(path(doc, "metrics", "social_echo", "reddit")))
      add("mw_document_tags", Some(jsArrStrings(path(doc, "custom", "tags")).mkString(", ")))
      add("mw_sentiment", jstr(path(doc, "enrichments", "sentiment")))
      add("mw_content_type", jstr(field(doc, "content_type")))
      add("mw_keywords", Some(jsArrStrings(path(doc, "matched", "keywords")).mkString(", ")))
      add("mw_keyphrases", Some(jsArrStrings(path(doc, "enrichments", "keyphrases")).mkString(", ")))
      add("mw_entities", Some(jsArrField(path(doc, "enrichments", "named_entities"), "name").mkString(", ")))
      add("mw_emv", jstr(path(doc, "metrics", "earned_media_value")))
      add("mw_views", jstr(path(doc, "metrics", "views")))
      add("mw_engagement_total", jstr(path(doc, "metrics", "engagement", "total")))
      add("mw_external_id", externalId)

      add("mw_search_id", jsArrField(matchedInputs, "id").headOption.orElse(queriedSearchId))
      add("mw_search_name", jsArrField(matchedInputs, "name").headOption)

      // keyphrases act as categories for DetectorFeed category filtering.
      val categories = jsArrStrings(path(doc, "enrichments", "keyphrases"))
      val images = jstr(path(doc, "content", "image")).filter(_.nonEmpty).toList

      Some(NewsPost(
        id = id,
        title = title,
        link = url,
        author = author,
        publishedDate = publishedDate,
        summary = Util.trunc(openingText,MeltwaterFeed.DEF_TEXT_LIMIT),
        source = this.source,
        typ = getSourceType(),
        categories = categories,
        images = images,
        feedMetadata = meta.toMap
      ))
    } catch {
      case e: Exception =>
        log.warn(s"MeltwaterFeed($source): failed to parse document: ${e.getMessage}")
        None
    }
  }

  private def parseIsoDate(dateStr: String): Long = {
    if (dateStr.isBlank) return System.currentTimeMillis()
    try {
      Instant.parse(dateStr).toEpochMilli
    } catch {
      case e: Exception =>
        log.warn(s"MeltwaterFeed($source): failed to parse date: '$dateStr': ${e.getMessage}")
        System.currentTimeMillis()      
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

  // ---------------------------------------------------------------------------
  // CSV parsing (Meltwater CSV export)
  // ---------------------------------------------------------------------------

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
      val url0 = row.getOrElse("URL", "")
      val openingText = row.getOrElse("Opening_Text", "")
      val source = row.getOrElse("Source", "")
      val influencer = row.getOrElse("Influencer", "")

      // Parse date: "2021-07-14 05:47:10"
      val publishedDate = parseMeltwaterDate(dateStr)

      val authorHandle =
        if (influencer.trim.nonEmpty) influencer.trim
        else twitterHandleFromProfileUrl(row.getOrElse("User_Profile_Url", ""))

      val (id, url) = MeltwaterFeed.resolveIdentity(
        source,
        url0,
        row.getOrElse("Tweet_id", ""),
        dateStr,
        headline,
        authorHandle
      )

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
  
}
