package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.syspulse.ext.sentinel.feeds.FeedId

class FeedIdSpec extends AnyFlatSpec with Matchers {

  "FeedId.stableId" should "slugify CryptoSlate URL to last path segment" in {
    val url = "https://cryptoslate.com/stablecoins-just-hit-a-record-322-billion-and-the-bank-run-warnings-are-getting-louder"
    FeedId.stableId(url) shouldBe "stablecoins-just-hit-a-record-322-billion-and-the-bank-run-warnings-are-getting-louder"
  }
}

