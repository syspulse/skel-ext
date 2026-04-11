package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration,FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try,Success,Failure}

import io.syspulse.skel.plugin.{Plugin,PluginDescriptor}
import io.syspulse.skel.util.Util

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.core.Event

import requests._
import ujson._


object DetectorAaveGov {
  val log = Logger(getClass.getName)

  //val GOVERNANCE_SUBGRAPH = "A7QMszgomC9cnnfpAcqZVLr2DffvkGNfimD8iUSMiurK"

  val VOTING_SUBGRAPHS = Map(
    "Ethereum" -> "2QPwuCfFtQ8WSCZoN3i9SmdoabMzbq2pmg4kRbrhymBV",
    "Polygon" -> "32WLrLTQctAgfoshbkteHfxLu3DpAeZwh2vUPWXV6Qxu",
    "Avalanche" -> "FngMWWGJV45McvV7GUBkrta9eoEi3sHZoH7MYnFQfZkr"
  )

  val STATE_MAP = Map(
    0 -> "Null", 1 -> "Created", 2 -> "Active", 3 -> "Queued",
    4 -> "Executed", 5 -> "Failed", 6 -> "Cancelled", 7 -> "Expired"
  )

  val STATE_ACTIVE = 2
  val STATE_EXECUTED = 4
  val STATE_CANCELLED = 6

  val DEF_PROPOSAL_COUNT = 5
  val DEF_PROPOSAL_IDS = ""
  val DEF_TRACK_ACTIVE = true
  val DEF_TRACK_CANCELLED = true
  val DEF_TRACK_EXECUTED = true
  val DEF_DESC = "{state}: {title}"

  def formatVotingPower(power: String): Double = {
    try {
      BigDecimal(power)./(BigDecimal("1000000000000000000")).toDouble
    } catch {
      case _: Exception => 0.0
    }
  }

  /** Normalize a timestamp value to ms: numeric (seconds or ms) or string -> Long; invalid/missing -> None. */
  private def parseTimestampToMs(ts: ujson.Value): Option[Long] = {
    if (ts == null || ts.isNull) return None
    val raw = ts match {
      case n: ujson.Num => Some(n.value.toLong)
      case s: ujson.Str => Try(s.value.toLong).toOption
      case _ => None
    }
    raw.filter(_ > 0).map { ms => if (ms < 1e12) ms * 1000L else ms }
  }

  /** Extract timestamp (ms): prefers proposal.transactions.created.timestamp (TransactionData), then proposal.proposalMetadata.timestamp, else System.currentTimeMillis(). */
  def getTimestampFromProposalMetadata(proposal: ujson.Value): Long = {
    def getTs(from: ujson.Value): Option[Long] =
      for {
        ts <- Try(from("timestamp")).toOption
        ms <- parseTimestampToMs(ts)
      } yield ms
    val fromCreated = for {
      transactions <- Try(proposal("transactions")).toOption
      if transactions != null && !transactions.isNull
      created <- Try(transactions("created")).toOption
      if created != null && !created.isNull
      ms <- getTs(created)
    } yield ms
    val fromMeta = for {
      meta <- Try(proposal("proposalMetadata")).toOption
      if meta != null && !meta.isNull
      ms <- getTs(meta)
    } yield ms
    try {
      fromCreated.orElse(fromMeta).getOrElse(System.currentTimeMillis())
    } catch {      
      case _: Exception =>
        System.currentTimeMillis()
    }
  }

  def graphqlQuery(subgraph: String, endpoint: String, query: String): Try[ujson.Value] = Try {
    val payload = ujson.Obj("query" -> query)
    log.info(s"${subgraph}: '$payload' -> ${endpoint}")
    val response = post(
      endpoint,
      data = ujson.write(payload),
      headers = Map("Content-Type" -> "application/json"),
      readTimeout = 10000,
      connectTimeout = 10000
    )
    if(response.statusCode != 200) {
      throw new Exception(s"${subgraph}: Failed to fetch proposals: ${response.statusCode} ${response.text()}")
    }

    val body = response.text()
    log.debug(s"${subgraph}: ${response.statusCode}: '$body'")
    ujson.read(body)
  }

  def fetchRecentProposals(subgraph: String, apiKey: String, count: Int = 5): Try[Seq[ujson.Value]] = {
    val endpoint = s"https://gateway-arbitrum.network.thegraph.com/api/$apiKey/subgraphs/id/$subgraph"

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

    graphqlQuery(subgraph, endpoint, query).map { result => {      
      result("data")("proposals").arr.toSeq
    }}
  }

  def fetchSpecificProposals(subgraph: String, apiKey: String, proposalIds: Seq[String]): Try[Seq[ujson.Value]] = {
    val endpoint = s"https://gateway-arbitrum.network.thegraph.com/api/$apiKey/subgraphs/id/$subgraph"

    // Fetch proposals one by one and collect results
    val results = proposalIds.flatMap { proposalId =>
      val query = s"""
      {
        proposal(id: "$proposalId") {
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

      val result = graphqlQuery(subgraph, endpoint, query) 
      log.debug(s"${subgraph} (${Util.trunc(apiKey, 10)}): '$result'")
      result match {
        case Success(result) =>
          val proposal = result("data")("proposal")
          if (!proposal.isNull) Some(proposal) else None
        case Failure(e) => 
          log.error(s"${subgraph}: Failed to fetch proposal",e)
          None
      }
    }

    Success(results)
  }

  def fetchVotesFromChain(apiKey: String, chain: String, subgraphId: String, proposalId: String): Seq[ujson.Obj] = {
    val endpoint = s"https://gateway-arbitrum.network.thegraph.com/api/$apiKey/subgraphs/id/$subgraphId"
    val query = s"""
    {
      voteEmitteds(
        where: {proposalId: "$proposalId"}
        first: 100
        orderBy: votingPower
        orderDirection: desc
      ) {
        voter
        support
        votingPower
      }
    }
    """

    try {
      val result = graphqlQuery(subgraphId, endpoint, query)
      result match {
        case Success(data) =>
          data("data")("voteEmitteds").arr.map { v =>
            ujson.Obj(
              "voter" -> v("voter").str,
              "support" -> v("support").bool,
              "votingPower" -> v("votingPower").str,
              "chain" -> chain
            )
          }.toSeq
        case Failure(e) => 
          log.error(s"${subgraphId}: Failed to fetch proposal",e)
          Seq.empty
      }
    } catch {
      case e: Exception => 
        log.error(s"${subgraphId}: Failed to fetch proposal",e)
        Seq.empty
    }
  }
}

class DetectorAaveGov(pd: PluginDescriptor) extends Sentry0 with Plugin {
  override def did = pd.name
  override def toString = s"${this.getClass.getSimpleName}"

  override def getSettings(rx: SentryRun0): Map[String, Any] = {
    rx.getConfig().env match {
      case "test" => Map("_cron_rate_limit" -> 1 * 10 * 1000L) // 10 seconds for testing
      case "dev" => Map("_cron_rate_limit" -> 1 * 60 * 1000L) // 1 minute for dev
      case _ => Map("_cron_rate_limit" -> 10 * 60 * 1000L) // 10 minutes for production
    }
  }

  override def onInit(rx: SentryRun0, conf: DetectorConfig): Int = {
    super.onInit(rx, conf)
    log.info(s"${rx.getExtId()}: Initialized Aave Governance detector")
    SentryRun.SENTRY_INIT
  }

  override def onStart(rx: SentryRun0, conf: DetectorConfig): Int = {
    super.onStart(rx, conf)
    onUpdate(rx, conf)
  }

  override def onUpdate(rx: SentryRun0, conf: DetectorConfig): Int = {
    // Store API key from config
    val defApiKey = rx.getConfiguration()(c => c.getString("thegraph.api.key")).getOrElse("")
    val apiKey = DetectorConfig.getString(rx.conf, "api_key").filter(!_.isBlank).getOrElse(defApiKey)
    rx.set("api_key", apiKey)

    val subgraph = rx.getConfiguration()(c => c.getString("thegraph.subgraph")).getOrElse("")
    rx.set("subgraph", subgraph)

    // Number of recent proposals to check (default 5)
    val proposalCount = DetectorConfig.getInt(rx.conf, "proposal_count", DetectorAaveGov.DEF_PROPOSAL_COUNT)
    rx.set("proposal_count", proposalCount)

    // Specific proposal IDs (semicolon-separated)
    val proposalIds = DetectorConfig.getString(rx.conf, "proposal_ids", DetectorAaveGov.DEF_PROPOSAL_IDS)
    rx.set("proposal_ids", proposalIds)

    // Track configuration
    val trackActive = DetectorConfig.getBoolean(rx.conf, "track_active", DetectorAaveGov.DEF_TRACK_ACTIVE)
    rx.set("track_active", trackActive)

    val trackCancelled = DetectorConfig.getBoolean(rx.conf, "track_cancelled", DetectorAaveGov.DEF_TRACK_CANCELLED)
    rx.set("track_cancelled", trackCancelled)

    val trackExecuted = DetectorConfig.getBoolean(rx.conf, "track_executed", DetectorAaveGov.DEF_TRACK_EXECUTED)
    rx.set("track_executed", trackExecuted)

    // Description template
    val desc = DetectorConfig.getString(rx.conf, "desc", DetectorAaveGov.DEF_DESC)
    rx.set("desc", desc)

    log.info(s"${rx.getExtId()}: Updated config - proposal_count=$proposalCount, proposal_ids='$proposalIds', track_active=$trackActive, track_cancelled=$trackCancelled, track_executed=$trackExecuted")
    SentryRun.SENTRY_RUNNING
  }

  override def onCron(rx: SentryRun0, elapsed: Long): Seq[Event] = {
    val subgraph = rx.get("subgraph").asInstanceOf[Option[String]].getOrElse("")
    if (subgraph.isEmpty) {
      log.error(s"${rx.getExtId()}: Subgraph is not set")
      return Seq.empty
    }

    val apiKey = rx.get("api_key").get.asInstanceOf[String]
    val proposalCount = rx.get("proposal_count").asInstanceOf[Option[Int]].getOrElse(DetectorAaveGov.DEF_PROPOSAL_COUNT)
    val proposalIdsStr = rx.get("proposal_ids").asInstanceOf[Option[String]].getOrElse(DetectorAaveGov.DEF_PROPOSAL_IDS)
    val trackActive = rx.get("track_active").asInstanceOf[Option[Boolean]].getOrElse(DetectorAaveGov.DEF_TRACK_ACTIVE)
    val trackCancelled = rx.get("track_cancelled").asInstanceOf[Option[Boolean]].getOrElse(DetectorAaveGov.DEF_TRACK_CANCELLED)
    val trackExecuted = rx.get("track_executed").asInstanceOf[Option[Boolean]].getOrElse(DetectorAaveGov.DEF_TRACK_EXECUTED)
    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorAaveGov.DEF_DESC)    

    log.info(s"${rx.getExtId()}: Checking Aave governance proposals...")

    // Determine which proposals to fetch
    val proposalsResult = if (!proposalIdsStr.isBlank && !proposalIdsStr.trim.isBlank) {
      val ids = proposalIdsStr.split(",").map(_.trim).filter(!_.isBlank)
      log.info(s"${rx.getExtId()}: Fetching specific proposals: ${ids.mkString(", ")}")
      DetectorAaveGov.fetchSpecificProposals(subgraph, apiKey, ids.toSeq)
    } else {
      log.info(s"${rx.getExtId()}: Fetching last $proposalCount proposals")
      DetectorAaveGov.fetchRecentProposals(subgraph, apiKey, proposalCount)
    }

    proposalsResult match {
      case Success(proposals) if proposals.nonEmpty =>
        log.info(s"${rx.getExtId()}: Found ${proposals.size} proposal(s)")

        proposals.flatMap { proposal =>
          try {
            val proposalId = proposal("proposalId").str
            val title = proposal("proposalMetadata")("title").str
            val creator = proposal("creator").str
            val stateCode = proposal("state").num.toInt
            val state = DetectorAaveGov.STATE_MAP.getOrElse(stateCode, "Unknown")

            val eventTs = DetectorAaveGov.getTimestampFromProposalMetadata(proposal)

            // Determine if we should process this proposal based on state and config
            val shouldProcess = stateCode match {
              case DetectorAaveGov.STATE_ACTIVE => trackActive
              case DetectorAaveGov.STATE_CANCELLED => trackCancelled
              case DetectorAaveGov.STATE_EXECUTED => trackExecuted
              case _ => false
            }

            if (!shouldProcess) {
              log.debug(s"${rx.getExtId()}: Skipping proposal #$proposalId (state=$state, tracking disabled)")
              None
            } else {
              val votes = proposal("votes")
              val votesForAggregate = if (votes.isNull) 0.0 else DetectorAaveGov.formatVotingPower(votes("forVotes").str)
              val votesAgainstAggregate = if (votes.isNull) 0.0 else DetectorAaveGov.formatVotingPower(votes("againstVotes").str)

              // Fetch individual votes from all chains
              val allVotes = DetectorAaveGov.VOTING_SUBGRAPHS.flatMap { case (chain, subgraphId) =>
                DetectorAaveGov.fetchVotesFromChain(apiKey, chain, subgraphId, proposalId)
              }.toSeq

              val votesForIndividual = allVotes.filter(_("support").bool).map(v => DetectorAaveGov.formatVotingPower(v("votingPower").str)).sum
              val votesAgainstIndividual = allVotes.filterNot(_("support").bool).map(v => DetectorAaveGov.formatVotingPower(v("votingPower").str)).sum

              // Use individual vote totals if aggregate is 0 (not bridged yet)
              val (votesFor, votesAgainst, voteSource) =
                if (votesForAggregate == 0.0 && votesAgainstAggregate == 0.0 && allVotes.nonEmpty) {
                  (votesForIndividual, votesAgainstIndividual, "individual votes (not bridged)")
                } else {
                  (votesForAggregate, votesAgainstAggregate, "aggregate (bridged)")
                }

              val totalVotes = votesFor + votesAgainst
              val differential = votesFor - votesAgainst
              val yesPercent = if (totalVotes > 0) votesFor / totalVotes * 100 else 0.0

              // Get quorum from votingConfig
              val votingConfig = proposal("votingConfig")
              val quorum = if (votingConfig.isNull) 0.0 else DetectorAaveGov.formatVotingPower(votingConfig("yesThreshold").str)
              val quorumMet = totalVotes >= quorum

              val chainsWithVotes = allVotes.map(_("chain").str).distinct.mkString(", ")

              // Determine severity based on state and conditions
              val (severity, alertReason) = stateCode match {
                case DetectorAaveGov.STATE_CANCELLED =>
                  (Severity.HIGH, "CANCELLED")

                case DetectorAaveGov.STATE_EXECUTED =>
                  if (!quorumMet) {
                    (Severity.HIGH, "Quorum NOT reached")
                  } else if (votesAgainst > votesFor) {
                    (Severity.HIGH, "No > Yes")
                  } else {
                    (Severity.MEDIUM, "EXECUTED")
                  }

                case DetectorAaveGov.STATE_ACTIVE =>
                  (Severity.INFO, "ACTIVE - voting in progress")

                case _ =>
                  (Severity.INFO, s"Proposal in State: $state")
              }

              // Generate deterministic event ID based on state
              val eid0 = stateCode match {
                case DetectorAaveGov.STATE_CANCELLED | DetectorAaveGov.STATE_EXECUTED =>
                  // Deterministic: did + proposalId + extId only
                  s"${did}-${proposalId}-${rx.getExtId()}"

                case DetectorAaveGov.STATE_ACTIVE =>
                  // Changes with votes: did + proposalId + extId + votes
                  s"${did}-${proposalId}-${rx.getExtId()}-${votesFor.toLong}-${votesAgainst.toLong}"

                case _ =>
                  // Other states: did + proposalId + extId + stateCode
                  s"${did}-${proposalId}-${rx.getExtId()}-${stateCode}"
              }

              val eid = Util.sha256(eid0)

              log.info(s"${rx.getExtId()}: Proposal #$proposalId [$state]: $title - YES: ${votesFor.toLong} AAVE (${yesPercent.toInt}%), NO: ${votesAgainst.toLong} AAVE - Severity: $severity - $alertReason")

              Some(EventUtil.createEvent(
                did = did,
                tx = None,
                monitoredAddr = rx.getAddr(),
                conf = Some(rx.getConf()),
                meta = Map(
                  "desc" -> desc,
                  "id" -> proposalId,
                  "title" -> title,
                  "author" -> creator,
                  "state" -> state,
                  "state_code" -> stateCode.toString,
                  "vote_yes" -> votesFor.toString,
                  "vote_no" -> votesAgainst.toString,
                  "vote_total" -> totalVotes.toString,
                  "vote_yes_percentage" -> f"$yesPercent%.2f",
                  "vote_diff" -> differential.toString,
                  "vote_quorum" -> quorum.toString,
                  "quorum" -> quorumMet.toString,
                  "vote_count" -> allVotes.size.toString,
                  "vote_source" -> voteSource,
                  "vote_chains" -> (if (chainsWithVotes.nonEmpty) chainsWithVotes else "none"),
                  "reason" -> alertReason,
                  "link" -> s"https://vote.onaave.com/proposal/?proposalId=${proposalId}",
                  "tx_hash" -> proposalId
                ),
                sev = Some(severity),
                eid0 = Some(eid),
                detectorTs = eventTs.toString
              ))
            }
          } catch {
            case e: Exception =>
              log.error(s"${rx.getExtId()}: Error processing proposal: ${e.getMessage}")
              None
          }
        }

      case Success(proposals) =>
        log.info(s"${rx.getExtId()}: No proposals found")
        Seq.empty

      case Failure(e) =>
        log.error(s"${rx.getExtId()}: Failed to fetch proposals: ${e.getMessage}")
        error(s"Failed to fetch Aave governance proposals", Some(e.getMessage))
        Seq.empty
    }
  }
}
