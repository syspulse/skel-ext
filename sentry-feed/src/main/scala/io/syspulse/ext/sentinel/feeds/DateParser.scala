package io.syspulse.ext.sentinel.feeds

import java.time.{ZonedDateTime, Instant}
import java.time.format.DateTimeFormatter
import java.util.Locale

object DateParser {
  // RFC-822 format used by RSS: "Fri, 12 Dec 2025 16:57:28 +0000"
  private val RFC822_FORMATTER = DateTimeFormatter
    .ofPattern("EEE, dd MMM yyyy HH:mm:ss Z", Locale.ENGLISH)

  // ISO-8601 format used by Atom: "2025-12-02T20:24:36+00:00"
  private val ISO8601_FORMATTER = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  def parseRfc822(dateStr: String): Long = {
    try {
      ZonedDateTime.parse(dateStr, RFC822_FORMATTER).toInstant.toEpochMilli
    } catch {
      case e: Exception =>
        // Fallback to current time
        System.currentTimeMillis()
    }
  }

  def parseIso8601(dateStr: String): Long = {
    try {
      ZonedDateTime.parse(dateStr, ISO8601_FORMATTER).toInstant.toEpochMilli
    } catch {
      case e: Exception =>
        System.currentTimeMillis()
    }
  }

  def formatTimestamp(ts: Long): String = {
    val instant = Instant.ofEpochMilli(ts)
    DateTimeFormatter.ISO_INSTANT.format(instant)
  }
}
