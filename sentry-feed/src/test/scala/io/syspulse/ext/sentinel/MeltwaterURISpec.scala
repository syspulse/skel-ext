package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.syspulse.ext.sentinel.feeds.MeltwaterURI

class MeltwaterURISpec extends AnyFlatSpec with Matchers {

  "MeltwaterURI" should "parse a single id with apiKey option" in {
    val u = MeltwaterURI("meltwater://100?apiKey=KEY")
    u.ids shouldBe Seq("100")
    u.apiKey shouldBe "KEY"
    u.ops should contain ("apiKey" -> "KEY")
  }

  it should "parse multiple ids with apiKey option" in {
    val u = MeltwaterURI("meltwater://28737363,28739045?apiKey=KEY")
    u.ids shouldBe Seq("28737363", "28739045")
    u.apiKey shouldBe "KEY"
  }

  it should "parse ids without apiKey (fallback to env)" in {
    val key = sys.env.getOrElse("MELTWATER_API_KEY", "")
    val u = MeltwaterURI("meltwater://100")
    u.ids shouldBe Seq("100")
    u.ops shouldBe Map.empty
    u.apiKey shouldBe key
  }

  it should "parse empty ids (list-all mode) with apiKey option" in {
    val u = MeltwaterURI("meltwater://?apiKey=KEY")
    u.ids shouldBe empty
    u.apiKey shouldBe "KEY"
  }

  it should "parse empty ids (list-all mode) without options" in {
    val key = sys.env.getOrElse("MELTWATER_API_KEY", "")
    val u = MeltwaterURI("meltwater://")
    u.ids shouldBe empty
    u.apiKey shouldBe key
  }

  it should "NOT treat the value after the prefix as apiKey" in {
    // apiKey is ALWAYS an option, never positioned after the prefix.
    val u = MeltwaterURI("meltwater://KEY123")
    u.ids shouldBe Seq("KEY123")
    u.ops.get("apiKey") shouldBe None
  }

  it should "preserve extra options" in {
    val u = MeltwaterURI("meltwater://100?apiKey=KEY&window=24h&page_size=50")
    u.ids shouldBe Seq("100")
    u.apiKey shouldBe "KEY"
    u.ops should contain allOf ("apiKey" -> "KEY", "window" -> "24h", "page_size" -> "50")
  }

  it should "resolve ${ENV} substitution in apiKey" in {
    val key = sys.env.getOrElse("MELTWATER_API_KEY", "")
    val u = MeltwaterURI("meltwater://100?apiKey=$" + "{MELTWATER_API_KEY}")
    u.apiKey shouldBe key
  }

  it should "trim whitespace around comma-separated ids" in {
    val u = MeltwaterURI("meltwater://100, 200 ,300?apiKey=KEY")
    u.ids shouldBe Seq("100", "200", "300")
  }
}
