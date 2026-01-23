package io.syspulse.ext.sentinel

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import spray.json._
import scala.io.Source

class SnapshotSpec extends AnyWordSpec with Matchers {

  "SnapshotJsonProtocol" should {
    "parse proposal response correctly" in {
      import SnapshotJsonProtocol._

      val jsonText = """{
        "id": "0x1234567890abcdef",
        "title": "Test Proposal",
        "body": "This is a test proposal",
        "choices": ["For", "Against", "Abstain"],
        "start": 1640000000,
        "end": 1640086400,
        "snapshot": "14000000",
        "state": "active",
        "author": "0xabcdef1234567890",
        "space": {
          "id": "aavedao.eth",
          "name": "Aave DAO"
        },
        "scores": [1000.0, 500.0, 100.0],
        "scores_total": 1600.0,
        "quorum": 500.0,
        "votes": 25
      }"""

      val proposal = jsonText.parseJson.convertTo[SnapshotProposal]

      proposal.id shouldBe "0x1234567890abcdef"
      proposal.title shouldBe "Test Proposal"
      proposal.body shouldBe "This is a test proposal"
      proposal.choices should have length 3
      proposal.choices.head shouldBe "For"
      proposal.start shouldBe 1640000000L
      proposal.end shouldBe 1640086400L
      proposal.snapshot shouldBe "14000000"
      proposal.state shouldBe "active"
      proposal.author shouldBe "0xabcdef1234567890"
      proposal.space.id shouldBe "aavedao.eth"
      proposal.space.name shouldBe "Aave DAO"
      proposal.scores shouldBe Some(Seq(1000.0, 500.0, 100.0))
      proposal.scores_total shouldBe Some(1600.0)
      proposal.quorum shouldBe Some(500.0)
      proposal.votes shouldBe Some(25)
    }

    "handle proposal with minimal fields" in {
      import SnapshotJsonProtocol._

      val jsonText = """{
        "id": "0xminimal",
        "title": "Minimal Proposal",
        "start": 1640000000,
        "end": 1640086400,
        "state": "pending",
        "author": "0x1111111111111111",
        "space": {
          "id": "test.eth",
          "name": "Test Space"
        }
      }"""

      val proposal = jsonText.parseJson.convertTo[SnapshotProposal]

      proposal.id shouldBe "0xminimal"
      proposal.title shouldBe "Minimal Proposal"
      proposal.body shouldBe ""
      proposal.choices shouldBe Seq.empty
      proposal.scores shouldBe None
      proposal.scores_total shouldBe None
      proposal.quorum shouldBe None
      proposal.votes shouldBe None
    }

    "parse vote response correctly" in {
      import SnapshotJsonProtocol._

      val jsonText = """{
        "id": "0xvote123",
        "voter": "0xvoter1234567890",
        "vp": 1234.56,
        "vp_by_strategy": [1234.56],
        "vp_state": "final",
        "created": 1640050000,
        "choice": 1,
        "space": {
          "id": "aavedao.eth",
          "name": "Aave DAO"
        }
      }"""

      val vote = jsonText.parseJson.convertTo[SnapshotVote]

      vote.id shouldBe "0xvote123"
      vote.voter shouldBe "0xvoter1234567890"
      vote.vp shouldBe 1234.56 +- 0.01
      vote.vp_by_strategy shouldBe Some(Seq(1234.56))
      vote.vp_state shouldBe Some("final")
      vote.created shouldBe 1640050000L
      vote.choice shouldBe JsNumber(1)
      vote.space.id shouldBe "aavedao.eth"
    }

    "handle vote with complex choice object" in {
      import SnapshotJsonProtocol._

      val jsonText = """{
        "id": "0xvote456",
        "voter": "0xvoter456",
        "vp": 500.0,
        "created": 1640050000,
        "choice": {"1": 30, "2": 70},
        "space": {
          "id": "test.eth",
          "name": "Test"
        }
      }"""

      val vote = jsonText.parseJson.convertTo[SnapshotVote]

      vote.choice shouldBe a[JsObject]
      vote.vp shouldBe 500.0 +- 0.01
    }
  }

  "DetectorSnapshotGov utility functions" should {
    "calculate vote distribution correctly" in {
      val proposal = SnapshotProposal(
        id = "test",
        title = "Test",
        body = "",
        choices = Seq("For", "Against", "Abstain"),
        start = 0,
        end = 0,
        snapshot = "",
        state = "active",
        author = "",
        space = SnapshotSpace("test.eth", "Test"),
        scores = Some(Seq(1000.0, 500.0, 100.0)),
        scores_total = Some(1600.0),
        quorum = Some(500.0),
        votes = Some(25)
      )

      val distribution = DetectorSnapshotGov.calculateVoteDistribution(proposal)

      distribution should have size 3
      distribution("For") shouldBe 1000.0
      distribution("Against") shouldBe 500.0
      distribution("Abstain") shouldBe 100.0
    }

    "handle empty scores" in {
      val proposal = SnapshotProposal(
        id = "test",
        title = "Test",
        body = "",
        choices = Seq("For", "Against"),
        start = 0,
        end = 0,
        snapshot = "",
        state = "pending",
        author = "",
        space = SnapshotSpace("test.eth", "Test"),
        scores = None,
        scores_total = None,
        quorum = None,
        votes = None
      )

      val distribution = DetectorSnapshotGov.calculateVoteDistribution(proposal)

      distribution shouldBe empty
    }

    "get winning choice correctly" in {
      val proposal = SnapshotProposal(
        id = "test",
        title = "Test",
        body = "",
        choices = Seq("For", "Against", "Abstain"),
        start = 0,
        end = 0,
        snapshot = "",
        state = "closed",
        author = "",
        space = SnapshotSpace("test.eth", "Test"),
        scores = Some(Seq(1000.0, 500.0, 100.0)),
        scores_total = Some(1600.0),
        quorum = Some(500.0),
        votes = Some(25)
      )

      val winning = DetectorSnapshotGov.getWinningChoice(proposal)

      winning shouldBe defined
      winning.get._1 shouldBe "For"
      winning.get._2 shouldBe 1000.0
    }

    "handle no winning choice when scores are empty" in {
      val proposal = SnapshotProposal(
        id = "test",
        title = "Test",
        body = "",
        choices = Seq("For", "Against"),
        start = 0,
        end = 0,
        snapshot = "",
        state = "pending",
        author = "",
        space = SnapshotSpace("test.eth", "Test"),
        scores = None,
        scores_total = None,
        quorum = None,
        votes = None
      )

      val winning = DetectorSnapshotGov.getWinningChoice(proposal)

      winning shouldBe None
    }
  }

  "Snapshot" should {
    "parse choice values correctly" in {
      val api = new Snapshot()
      val choices = Seq("For", "Against", "Abstain")

      // Single choice
      api.parseChoiceValue(JsNumber(1), choices) shouldBe "For"
      api.parseChoiceValue(JsNumber(2), choices) shouldBe "Against"
      api.parseChoiceValue(JsNumber(3), choices) shouldBe "Abstain"

      // Array of choices
      val arrayChoice = JsArray(JsNumber(1), JsNumber(3))
      api.parseChoiceValue(arrayChoice, choices) shouldBe "For, Abstain"

      // Object (weighted voting)
      val objChoice = JsObject("1" -> JsNumber(60), "2" -> JsNumber(40))
      val result = api.parseChoiceValue(objChoice, choices)
      result should (include("1:") and include("2:"))
    }

    "handle out of bounds choice index" in {
      val api = new Snapshot()
      val choices = Seq("For", "Against")

      api.parseChoiceValue(JsNumber(5), choices) shouldBe "Choice 5"
    }
  }
}
