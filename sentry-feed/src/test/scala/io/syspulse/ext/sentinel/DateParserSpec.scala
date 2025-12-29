package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.syspulse.ext.sentinel.feeds.DateParser

class DateParserSpec extends AnyFlatSpec with Matchers {

  "DateParser" should "parse RFC-822 date format" in {
    val dateStr = "Fri, 12 Dec 2025 16:57:28 +0000"
    val timestamp = DateParser.parseRfc822(dateStr)

    timestamp should be > 0L
    // Verify it's a reasonable timestamp (should be in 2025)
    timestamp should be > 1704067200000L // Jan 1, 2024
    timestamp should be < 1893456000000L // Jan 1, 2030
  }

  it should "parse ISO-8601 date format" in {
    val dateStr = "2025-12-02T20:24:36+00:00"
    val timestamp = DateParser.parseIso8601(dateStr)

    timestamp should be > 0L
    // Verify it's a reasonable timestamp
    timestamp should be > 1704067200000L // Jan 1, 2024
    timestamp should be < 1893456000000L // Jan 1, 2030
  }

  it should "handle invalid RFC-822 date gracefully" in {
    val dateStr = "invalid date"
    val timestamp = DateParser.parseRfc822(dateStr)

    // Should fallback to current time
    timestamp should be > 0L
    val now = System.currentTimeMillis()
    // Should be within a few seconds of now
    Math.abs(timestamp - now) should be < 5000L
  }

  it should "handle invalid ISO-8601 date gracefully" in {
    val dateStr = "not a date"
    val timestamp = DateParser.parseIso8601(dateStr)

    // Should fallback to current time
    timestamp should be > 0L
    val now = System.currentTimeMillis()
    Math.abs(timestamp - now) should be < 5000L
  }

  it should "format timestamp to ISO instant" in {
    val timestamp = 1733160000000L // Example timestamp
    val formatted = DateParser.formatTimestamp(timestamp)

    formatted should not be empty
    formatted should include("T")
    formatted should include("Z")
    formatted should startWith("2024-") // Should be in 2024
  }

  it should "parse real RSS date from Cointelegraph" in {
    // Example from actual RSS feed
    val dateStr = "Thu, 12 Dec 2024 18:01:40 +0000"
    val timestamp = DateParser.parseRfc822(dateStr)

    timestamp should be > 0L
    // December 2024
    timestamp should be > 1733097600000L // Dec 1, 2024
    timestamp should be < 1735689600000L // Jan 1, 2025
  }

  it should "parse real Atom date from Reddit" in {
    // Example from actual Reddit feed
    val dateStr = "2024-12-02T20:24:36+00:00"
    val timestamp = DateParser.parseIso8601(dateStr)

    timestamp should be > 0L
    // December 2024
    timestamp should be > 1733097600000L // Dec 1, 2024
    timestamp should be < 1735689600000L // Jan 1, 2025
  }
}
