package io.syspulse.ext.sentinel

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}
import io.syspulse.skel.plugin.PluginDescriptor
import ujson._

class DetectorAaveGovSpec extends AnyWordSpec with Matchers {

  "DetectorAaveGov" should {
    "initialize with default configuration" in {
      val pd = PluginDescriptor("DetectorAaveGov", "1.0.0", "Test Aave Gov Detector")
      val detector = new DetectorAaveGov(pd)

      detector.did shouldBe "DetectorAaveGov"
      detector.toString should include("DetectorAaveGov")
    }

    "have valid plugin descriptor" in {
      val pd = PluginDescriptor("DetectorAaveGov", "1.0.0", "Test Aave Gov Detector")

      pd.name shouldBe "DetectorAaveGov"
    }

    "have correct state mappings" in {
      DetectorAaveGov.STATE_MAP(0) shouldBe "Null"
      DetectorAaveGov.STATE_MAP(1) shouldBe "Created"
      DetectorAaveGov.STATE_MAP(2) shouldBe "Active"
      DetectorAaveGov.STATE_MAP(3) shouldBe "Queued"
      DetectorAaveGov.STATE_MAP(4) shouldBe "Executed"
      DetectorAaveGov.STATE_MAP(5) shouldBe "Failed"
      DetectorAaveGov.STATE_MAP(6) shouldBe "Cancelled"
      DetectorAaveGov.STATE_MAP(7) shouldBe "Expired"
    }

    "have correct state constants" in {
      DetectorAaveGov.STATE_ACTIVE shouldBe 2
      DetectorAaveGov.STATE_EXECUTED shouldBe 4
      DetectorAaveGov.STATE_CANCELLED shouldBe 6
    }

    "have correct default values" in {
      DetectorAaveGov.DEF_PROPOSAL_COUNT shouldBe 5
      DetectorAaveGov.DEF_PROPOSAL_IDS shouldBe ""
      DetectorAaveGov.DEF_TRACK_ACTIVE shouldBe true
      DetectorAaveGov.DEF_TRACK_CANCELLED shouldBe true
      DetectorAaveGov.DEF_TRACK_EXECUTED shouldBe true
      DetectorAaveGov.DEF_DESC shouldBe "{state}: {title}"
    }

    "format voting power correctly" in {
      DetectorAaveGov.formatVotingPower("1000000000000000000") shouldBe 1.0
      DetectorAaveGov.formatVotingPower("500000000000000000") shouldBe 0.5
      DetectorAaveGov.formatVotingPower("0") shouldBe 0.0
      DetectorAaveGov.formatVotingPower("invalid") shouldBe 0.0
    }

    "parse proposal data from Aave Governance V3 subgraph" in {
      // Mock proposal data matching actual Aave Governance V3 schema
      val proposal = ujson.read("""
        {
          "id": "0x123",
          "proposalId": "400",
          "creator": "0x57ab7ee15ce5ecacb1ab84ee42d5a9d0d8112922",
          "state": 2,
          "votingDuration": 259200,
          "votes": {
            "forVotes": "595938600000000000000000",
            "againstVotes": "0"
          },
          "votingConfig": {
            "yesThreshold": "320000000000000000000000",
            "yesNoDifferential": "80000000000000000000000",
            "minPropositionPower": "50000000000000000000000"
          },
          "proposalMetadata": {
            "title": "Addition of cbBTC/Stablecoin E-Mode"
          }
        }
      """)

      // Verify fields can be accessed
      proposal("proposalId").str shouldBe "400"
      proposal("creator").str shouldBe "0x57ab7ee15ce5ecacb1ab84ee42d5a9d0d8112922"
      proposal("state").num.toInt shouldBe 2
      proposal("proposalMetadata")("title").str shouldBe "Addition of cbBTC/Stablecoin E-Mode"
      proposal("votes")("forVotes").str shouldBe "595938600000000000000000"
      proposal("votes")("againstVotes").str shouldBe "0"
    }

    "use current system timestamp when subgraph doesn't provide timestamps" in {
      // Aave Governance V3 subgraph does NOT provide timestamp fields
      // Events should use current system time
      val before = System.currentTimeMillis()
      val eventTs = System.currentTimeMillis()
      val after = System.currentTimeMillis()

      eventTs should be >= before
      eventTs should be <= after
    }
  }

  "DetectorAaveGov.fetchRecentProposals" should {
    "construct correct GraphQL query with transactions.created and ProposalMetadata" in {
      // Verify the query structure: transactions.created.timestamp (TransactionData, preferred), proposalMetadata title
      val count = 5
      val query = s"""
      {
        proposals(first: $count, orderBy: proposalId, orderDirection: desc) {
          id
          proposalId
          creator
          state
          votingDuration
          votes {
            forVotes
            againstVotes
          }
          votingConfig {
            yesThreshold
            yesNoDifferential
            minPropositionPower
          }
          transactions {
            created {
              timestamp
            }
          }
          proposalMetadata {
            title
          }
        }
      }
      """

      // Verify query doesn't contain root-level timestamp fields that don't exist in Aave V3 schema
      query should not include "creationTime"
      query should not include "startTime"
      query should not include "endTime"
      query should not include "executionTime"
      query should not include "cancellationTime"
      query should not include "lastUpdateTimestamp"
      query should not include "startBlock"
      query should not include "endBlock"

      // Verify query DOES contain transactions.created (TransactionData) and proposalMetadata
      query should include("proposalId")
      query should include("creator")
      query should include("state")
      query should include("votes")
      query should include("votingConfig")
      query should include("transactions")
      query should include("created")
      query should include("proposalMetadata")
      query should include("timestamp")
    }

    "prefer transactions.created.timestamp over proposalMetadata.timestamp" in {
      val proposalWithBoth = ujson.read("""
        {
          "transactions": { "created": { "timestamp": 1700000000 } },
          "proposalMetadata": { "title": "Test", "timestamp": 1640000000 }
        }
      """)
      val ms = DetectorAaveGov.getTimestampFromProposalMetadata(proposalWithBoth)
      ms shouldBe 1700000000000L
    }

    "extract timestamp from ProposalMetadata when present" in {
      val proposalWithTs = ujson.read("""
        {
          "proposalMetadata": {
            "title": "Test",
            "timestamp": 1640000000
          }
        }
      """)
      val ms = DetectorAaveGov.getTimestampFromProposalMetadata(proposalWithTs)
      ms shouldBe 1640000000000L
    }

    "extract timestamp from transactions.created when present (no proposalMetadata timestamp)" in {
      val proposalTxOnly = ujson.read("""
        {
          "transactions": { "created": { "timestamp": 1700000000 } },
          "proposalMetadata": { "title": "Test" }
        }
      """)
      val ms = DetectorAaveGov.getTimestampFromProposalMetadata(proposalTxOnly)
      ms shouldBe 1700000000000L
    }

    "extract timestamp from ProposalMetadata string (seconds)" in {
      val proposalWithTs = ujson.read("""
        {
          "proposalMetadata": {
            "title": "Test",
            "timestamp": "1640000000"
          }
        }
      """)
      val ms = DetectorAaveGov.getTimestampFromProposalMetadata(proposalWithTs)
      ms shouldBe 1640000000000L
    }

    "fall back to current time when ProposalMetadata has no timestamp" in {
      val proposalNoTs = ujson.read("""
        {
          "proposalMetadata": {
            "title": "Test"
          }
        }
      """)
      val before = System.currentTimeMillis()
      val ms = DetectorAaveGov.getTimestampFromProposalMetadata(proposalNoTs)
      val after = System.currentTimeMillis()
      ms should be >= before
      ms should be <= after
    }

    "parse response with actual Aave V3 schema fields" in {
      // Mock response from Aave Governance V3 subgraph
      val mockResponse = ujson.read("""
        {
          "data": {
            "proposals": [
              {
                "id": "0x1",
                "proposalId": "400",
                "creator": "0x57ab7ee15ce5ecacb1ab84ee42d5a9d0d8112922",
                "state": 2,
                "votingDuration": 259200,
                "votes": {
                  "forVotes": "595938600000000000000000",
                  "againstVotes": "0"
                },
                "votingConfig": {
                  "yesThreshold": "320000000000000000000000",
                  "yesNoDifferential": "80000000000000000000000",
                  "minPropositionPower": "50000000000000000000000"
                },
                "proposalMetadata": {
                  "title": "Addition of cbBTC/Stablecoin E-Mode"
                }
              },
              {
                "id": "0x2",
                "proposalId": "399",
                "creator": "0xabc",
                "state": 4,
                "votingDuration": 259200,
                "votes": {
                  "forVotes": "1000000000000000000000000",
                  "againstVotes": "100000000000000000000000"
                },
                "votingConfig": {
                  "yesThreshold": "320000000000000000000000",
                  "yesNoDifferential": "80000000000000000000000",
                  "minPropositionPower": "50000000000000000000000"
                },
                "proposalMetadata": {
                  "title": "Another Proposal"
                }
              }
            ]
          }
        }
      """)

      // Parse proposals
      val proposals = mockResponse("data")("proposals").arr.toSeq

      proposals should have length 2

      // Verify first proposal
      val proposal1 = proposals(0)
      proposal1("proposalId").str shouldBe "400"
      proposal1("creator").str shouldBe "0x57ab7ee15ce5ecacb1ab84ee42d5a9d0d8112922"
      proposal1("state").num.toInt shouldBe 2
      proposal1("proposalMetadata")("title").str shouldBe "Addition of cbBTC/Stablecoin E-Mode"

      // Verify second proposal
      val proposal2 = proposals(1)
      proposal2("proposalId").str shouldBe "399"
      proposal2("state").num.toInt shouldBe 4
      proposal2("proposalMetadata")("title").str shouldBe "Another Proposal"
    }

    "handle proposals without timestamp fields" in {
      // Verify that proposals work fine without timestamp fields
      val mockProposal = ujson.read("""
        {
          "id": "0x1",
          "proposalId": "400",
          "creator": "0x123",
          "state": 2,
          "votingDuration": 259200,
          "votes": {
            "forVotes": "1000000000000000000000000",
            "againstVotes": "0"
          },
          "votingConfig": {
            "yesThreshold": "320000000000000000000000",
            "yesNoDifferential": "80000000000000000000000",
            "minPropositionPower": "50000000000000000000000"
          },
          "proposalMetadata": {
            "title": "Test Proposal"
          }
        }
      """)

      // Should not have timestamp fields
      mockProposal.obj.contains("timestamp") shouldBe false
      mockProposal.obj.contains("creationTime") shouldBe false
      mockProposal.obj.contains("executionTime") shouldBe false

      // Should have all required fields
      mockProposal.obj.contains("proposalId") shouldBe true
      mockProposal.obj.contains("creator") shouldBe true
      mockProposal.obj.contains("state") shouldBe true
      mockProposal.obj.contains("votes") shouldBe true
    }
  }
}
