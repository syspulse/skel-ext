package io.syspulse.ext.sentinel

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import spray.json._
import scala.io.Source

class BubblemapsSpec extends AnyWordSpec with Matchers {
  private def readResource(path: String): String =
    Source
      .fromResource(path.stripPrefix("/"))
      .getLines()
      .mkString("\n")

  "BubblemapsJsonProtocol" should {
    "parse SHIB-like response correctly" in {
      import BubblemapsJsonProtocol._

      val jsonText = """{"metadata":{"dt_update":"2025-11-21T02:37:30.727236","ts_update":1763692650,"identified_supply":{"share_in_cexs":0.25,"share_in_dexs":0.0,"share_in_other_contracts":0.42}},"nodes":null,"relationships":null,"decentralization_score":57.51,"clusters":[{"share":0.03490022082813747,"amount":34899604348114.22,"holder_count":13,"holders":["0x2d7af085f2256f114c8a9f540969f0e0ab1c2e5e","0xe1474359c74e78fa8387d9cb58f393693e378de3"]}]}"""

      val apiResponse = jsonText.parseJson.convertTo[BubblemapsApiResponse]

      apiResponse.decentralization_score shouldBe 57.51
      apiResponse.clusters should have length 1
      apiResponse.nodes shouldBe None
      apiResponse.metadata.dt_update shouldBe "2025-11-21T02:37:30.727236"

      val firstCluster = apiResponse.clusters.head
      firstCluster.share shouldBe 0.03490022082813747 +- 0.0001
      firstCluster.holder_count shouldBe 13
      firstCluster.holders should have length 2
      firstCluster.holders.head shouldBe "0x2d7af085f2256f114c8a9f540969f0e0ab1c2e5e"
    }

    "handle null nodes field" in {
      import BubblemapsJsonProtocol._

      val json = """{"decentralization_score":50.0,"clusters":[],"nodes":null,"metadata":{"dt_update":"2025-11-21T00:00:00","ts_update":1732147200,"identified_supply":null},"relationships":null}"""
      val apiResponse = json.parseJson.convertTo[BubblemapsApiResponse]

      apiResponse.nodes shouldBe None
      apiResponse.decentralization_score shouldBe 50.0
    }

    "parse JESSE.json with nodes object correctly" in {
      import BubblemapsJsonProtocol._

      val jsonText = readResource("JESSE.json")
      val apiResponse = jsonText.parseJson.convertTo[BubblemapsApiResponse]

      apiResponse.decentralization_score shouldBe 78.57
      apiResponse.clusters should have length 1
      apiResponse.nodes shouldBe defined
      apiResponse.metadata.ts_update should be > 0L

      val nodes = apiResponse.nodes.get
      nodes.top_holders should not be empty
      nodes.top_holders.head.address shouldBe "0x50f88fe97f72cd3e75b9eb4f747f59bceba80d59"
      nodes.top_holders.head.address_details shouldBe defined
      nodes.top_holders.head.address_details.get.label shouldBe Some("jesse")
      nodes.top_holders.head.holder_data shouldBe defined
      nodes.top_holders.head.holder_data.get.amount shouldBe 499522929.5003422 +- 0.0001

      nodes.magic_nodes shouldBe defined
      nodes.magic_nodes.get should not be empty

      val firstCluster = apiResponse.clusters.head
      firstCluster.share shouldBe 0.0024788075226436996 +- 0.0001
      firstCluster.holder_count shouldBe 2
      firstCluster.holders should have length 2
    }

    "parse BubblemapsResponse from JESSE.json" in {
      import BubblemapsJsonProtocol._

      val jsonText = readResource("JESSE.json")
      val apiResponse = jsonText.parseJson.convertTo[BubblemapsApiResponse]

      val response = BubblemapsResponse(
        decentralization_score = apiResponse.decentralization_score,
        clusters = apiResponse.clusters,
        top_holders = apiResponse.nodes.map(_.top_holders).getOrElse(Seq.empty),
        metadata = apiResponse.metadata
      )

      response.decentralization_score shouldBe 78.57
      response.clusters should have length 1
      response.top_holders should not be empty
      response.top_holders.length should be > 50
    }
  }
}
