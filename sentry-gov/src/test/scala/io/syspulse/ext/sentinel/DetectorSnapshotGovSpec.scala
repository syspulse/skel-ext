package io.syspulse.ext.sentinel

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}
import io.syspulse.skel.plugin.PluginDescriptor
import io.hacken.ext.sentinel.{SentryRun, Sentry}
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.core.{Event, Severity}
import com.typesafe.config.ConfigFactory

class DetectorSnapshotGovSpec extends AnyWordSpec with Matchers {

  "DetectorSnapshotGov" should {
    "initialize with default configuration" in {
      val pd = PluginDescriptor("DetectorSnapshotGov", "1.0.0", "Test Snapshot Gov Detector")
      val detector = new DetectorSnapshotGov(pd)

      detector.did shouldBe "DetectorSnapshotGov"
      detector.toString should include("DetectorSnapshotGov")
    }

    "have valid plugin descriptor" in {
      val pd = PluginDescriptor("DetectorSnapshotGov", "1.0.0", "Test Snapshot Gov Detector")

      pd.name shouldBe "DetectorSnapshotGov"
    }

    "parse state constants correctly" in {
      DetectorSnapshotGov.STATE_ACTIVE shouldBe "active"
      DetectorSnapshotGov.STATE_CLOSED shouldBe "closed"
      DetectorSnapshotGov.STATE_PENDING shouldBe "pending"
    }

    "have correct default values" in {
      DetectorSnapshotGov.DEF_PROPOSAL_COUNT shouldBe 5
      DetectorSnapshotGov.DEF_PROPOSAL_IDS shouldBe ""
      DetectorSnapshotGov.DEF_SPACE shouldBe "aavedao.eth"
      DetectorSnapshotGov.DEF_TRACK_ACTIVE shouldBe true
      DetectorSnapshotGov.DEF_TRACK_CLOSED shouldBe true
      DetectorSnapshotGov.DEF_TRACK_PENDING shouldBe false
      DetectorSnapshotGov.DEF_DESC shouldBe "{state}: {title}"
      DetectorSnapshotGov.DEF_MIN_VP_FOR_ALERT shouldBe 1000.0
    }

    "calculate vote distribution correctly" in {
      val proposal = SnapshotProposal(
        id = "test-proposal-1",
        title = "Test Proposal",
        body = "Description",
        choices = Seq("Yes", "No", "Abstain"),
        start = 1640000000L,
        end = 1640086400L,
        snapshot = "14000000",
        state = "active",
        author = "0xauthor",
        space = SnapshotSpace("test.eth", "Test Space"),
        scores = Some(Seq(750.0, 200.0, 50.0)),
        scores_total = Some(1000.0),
        quorum = Some(500.0),
        votes = Some(15)
      )

      val distribution = DetectorSnapshotGov.calculateVoteDistribution(proposal)

      distribution should have size 3
      distribution("Yes") shouldBe 750.0
      distribution("No") shouldBe 200.0
      distribution("Abstain") shouldBe 50.0
    }

    "identify winning choice correctly" in {
      val proposal = SnapshotProposal(
        id = "test-proposal-2",
        title = "Another Proposal",
        body = "Description",
        choices = Seq("Option A", "Option B", "Option C"),
        start = 1640000000L,
        end = 1640086400L,
        snapshot = "14000000",
        state = "closed",
        author = "0xauthor",
        space = SnapshotSpace("test.eth", "Test Space"),
        scores = Some(Seq(300.0, 800.0, 100.0)),
        scores_total = Some(1200.0),
        quorum = Some(500.0),
        votes = Some(20)
      )

      val winning = DetectorSnapshotGov.getWinningChoice(proposal)

      winning shouldBe defined
      winning.get._1 shouldBe "Option B"
      winning.get._2 shouldBe 800.0
    }

    "handle proposal with no scores" in {
      val proposal = SnapshotProposal(
        id = "test-proposal-3",
        title = "Pending Proposal",
        body = "Description",
        choices = Seq("Yes", "No"),
        start = 1640000000L,
        end = 1640086400L,
        snapshot = "14000000",
        state = "pending",
        author = "0xauthor",
        space = SnapshotSpace("test.eth", "Test Space"),
        scores = None,
        scores_total = None,
        quorum = None,
        votes = None
      )

      val distribution = DetectorSnapshotGov.calculateVoteDistribution(proposal)
      distribution shouldBe empty

      val winning = DetectorSnapshotGov.getWinningChoice(proposal)
      winning shouldBe None
    }

    "handle edge cases in vote calculations" in {
      // Test with all zeros
      val proposalZero = SnapshotProposal(
        id = "test-zero",
        title = "Zero Votes",
        body = "",
        choices = Seq("A", "B"),
        start = 0,
        end = 0,
        snapshot = "",
        state = "active",
        author = "",
        space = SnapshotSpace("test.eth", "Test"),
        scores = Some(Seq(0.0, 0.0)),
        scores_total = Some(0.0),
        quorum = Some(100.0),
        votes = Some(0)
      )

      val winning = DetectorSnapshotGov.getWinningChoice(proposalZero)
      winning shouldBe defined
      // Both have 0, either could be winning
      winning.get._2 shouldBe 0.0
    }

    "verify settings based on environment" in {
      val pd = PluginDescriptor("DetectorSnapshotGov", "1.0.0", "Test")
      val detector = new DetectorSnapshotGov(pd)

      // We can't easily test SentryRun without the full framework,
      // but we can verify the detector instance is created
      detector should not be null
      detector.did shouldBe "DetectorSnapshotGov"
    }
  }

  "Snapshot integration" should {
    "be initialized correctly within detector" in {
      val pd = PluginDescriptor("DetectorSnapshotGov", "1.0.0", "Test")
      val detector = new DetectorSnapshotGov(pd)

      // Detector should be created successfully
      detector should not be null
    }
  }
}
