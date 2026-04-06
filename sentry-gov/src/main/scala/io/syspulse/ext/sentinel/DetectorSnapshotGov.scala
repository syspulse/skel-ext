package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration,FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try,Success,Failure}

import java.time.Instant

import io.syspulse.skel.plugin.{Plugin,PluginDescriptor}
import io.syspulse.skel.util.TimeUtil
import io.syspulse.skel.util.Util

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.core.Event

import requests._
import ujson._

object DetectorSnapshotGov {
  val DEF_PROPOSAL_COUNT = 5
  val DEF_PROPOSAL_IDS = ""
  val DEF_SPACE = "" //"aavedao.eth"
  val DEF_TRACK_ACTIVE = true
  val DEF_TRACK_CLOSED = true
  val DEF_TRACK_PENDING = false
  val DEF_DESC = "{state}: {title}"
  val DEF_MIN_VP_FOR_ALERT = 1000.0

  val STATE_PENDING = "pending"
  val STATE_ACTIVE = "active"
  val STATE_CLOSED = "closed"
  

  def calculateVoteDistribution(proposal: SnapshotProposal): Map[String, Double] = {
    val scores = proposal.scores.getOrElse(Seq.empty)
    val choices = proposal.choices

    if (scores.isEmpty || choices.isEmpty) {
      Map.empty
    } else {
      choices.zip(scores).toMap
    }
  }

  def getWinningChoice(proposal: SnapshotProposal): Option[(String, Double)] = {
    val distribution = calculateVoteDistribution(proposal)
    if (distribution.isEmpty) None
    else Some(distribution.maxBy(_._2))
  }

  def epochSecondToUtcIso(epochSec: Long): String = Instant.ofEpochSecond(epochSec).toString
}

class DetectorSnapshotGov(pd: PluginDescriptor) extends Sentry0 with Plugin {
  override def did = pd.name
  override def toString = s"${this.getClass.getSimpleName}"

  private var snapshotApi: Option[Snapshot] = None

  override def getSettings(rx: SentryRun0): Map[String, Any] = {
    rx.getConfig().env match {
      case "test" => Map("_cron_rate_limit" -> 1 * 10 * 1000L) // 10 seconds for testing
      case "dev" => Map("_cron_rate_limit" -> 1 * 60 * 1000L) // 1 minute for dev
      case _ => Map("_cron_rate_limit" -> 10 * 60 * 1000L) // 10 minutes for production
    }
  }

  override def onInit(rx: SentryRun0, conf: DetectorConfig): Int = {
    super.onInit(rx, conf)
    log.info(s"${rx.getExtId()}: Initialized Snapshot Governance detector")
    SentryRun.SENTRY_INIT
  }

  override def onStart(rx: SentryRun0, conf: DetectorConfig): Int = {
    super.onStart(rx, conf)
    onUpdate(rx, conf)
  }

  override def onUpdate(rx: SentryRun0, conf: DetectorConfig): Int = {
    // Store API key from config (optional for Snapshot)
    val defApiKey = rx.getConfiguration()(c => c.getString("snapshot.api.key")).getOrElse("")
    val apiKey = DetectorConfig.getString(rx.conf, "api_key", defApiKey)
    val apiKeyOpt = if (apiKey.nonEmpty) Some(apiKey) else None
    rx.set("api_key", apiKeyOpt)

    // Initialize Snapshot API
    snapshotApi = Some(new Snapshot(apiKeyOpt))

    // Snapshot space to monitor
    val space = DetectorConfig.getString(rx.conf, "space", DetectorSnapshotGov.DEF_SPACE)
    rx.set("space", space)

    // Number of recent proposals to check (default 5)
    val proposalCount = DetectorConfig.getInt(rx.conf, "proposal_count", DetectorSnapshotGov.DEF_PROPOSAL_COUNT)
    rx.set("proposal_count", proposalCount)

    // Specific proposal IDs (comma-separated)
    val proposalIds = DetectorConfig.getString(rx.conf, "proposal_ids", DetectorSnapshotGov.DEF_PROPOSAL_IDS)
    rx.set("proposal_ids", proposalIds)

    // Track configuration
    val trackActive = DetectorConfig.getBoolean(rx.conf, "track_active", DetectorSnapshotGov.DEF_TRACK_ACTIVE)
    rx.set("track_active", trackActive)

    val trackClosed = DetectorConfig.getBoolean(rx.conf, "track_closed", DetectorSnapshotGov.DEF_TRACK_CLOSED)
    rx.set("track_closed", trackClosed)

    val trackPending = DetectorConfig.getBoolean(rx.conf, "track_pending", DetectorSnapshotGov.DEF_TRACK_PENDING)
    rx.set("track_pending", trackPending)

    // Description template
    val desc = DetectorConfig.getString(rx.conf, "desc", DetectorSnapshotGov.DEF_DESC)
    rx.set("desc", desc)

    // Minimum voting power for alert on individual votes
    val minVpForAlert = DetectorConfig.getDouble(rx.conf, "min_vp_for_alert", DetectorSnapshotGov.DEF_MIN_VP_FOR_ALERT)
    rx.set("min_vp_for_alert", minVpForAlert)

    log.info(s"${rx.getExtId()}: Updated config - space=$space, proposal_count=$proposalCount, proposal_ids='$proposalIds', track_active=$trackActive, track_closed=$trackClosed, track_pending=$trackPending")
    SentryRun.SENTRY_RUNNING
  }

  override def onCron(rx: SentryRun0, elapsed: Long): Seq[Event] = {
    val space = rx.get("space").asInstanceOf[Option[String]].getOrElse(DetectorSnapshotGov.DEF_SPACE)
    val proposalCount = rx.get("proposal_count").asInstanceOf[Option[Int]].getOrElse(DetectorSnapshotGov.DEF_PROPOSAL_COUNT)
    val proposalIdsStr = rx.get("proposal_ids").asInstanceOf[Option[String]].getOrElse(DetectorSnapshotGov.DEF_PROPOSAL_IDS)
    val trackActive = rx.get("track_active").asInstanceOf[Option[Boolean]].getOrElse(DetectorSnapshotGov.DEF_TRACK_ACTIVE)
    val trackClosed = rx.get("track_closed").asInstanceOf[Option[Boolean]].getOrElse(DetectorSnapshotGov.DEF_TRACK_CLOSED)
    val trackPending = rx.get("track_pending").asInstanceOf[Option[Boolean]].getOrElse(DetectorSnapshotGov.DEF_TRACK_PENDING)
    val desc = rx.get("desc").asInstanceOf[Option[String]].getOrElse(DetectorSnapshotGov.DEF_DESC)
    val minVpForAlert = rx.get("min_vp_for_alert").asInstanceOf[Option[Double]].getOrElse(DetectorSnapshotGov.DEF_MIN_VP_FOR_ALERT)

    val api = snapshotApi.getOrElse {
      log.error(s"${rx.getExtId()}: Snapshot API not initialized")
      return Seq.empty
    }

    log.info(s"${rx.getExtId()}: Checking Snapshot governance proposals for space: $space")

    // Determine which proposals to fetch
    val proposalsResult = if (proposalIdsStr.nonEmpty && proposalIdsStr.trim.nonEmpty) {
      val ids = proposalIdsStr.split(",").map(_.trim).filter(_.nonEmpty)
      log.info(s"${rx.getExtId()}: Fetching specific proposals: ${ids.mkString(", ")}")
      api.fetchSpecificProposals(space, ids.toSeq)
    } else {
      log.info(s"${rx.getExtId()}: Fetching last $proposalCount proposals")
      api.fetchRecentProposals(space, proposalCount)
    }

    proposalsResult match {
      case Success(proposals) if proposals.nonEmpty =>
        log.info(s"${rx.getExtId()}: Found ${proposals.size} proposal(s)")

        proposals.flatMap { proposal =>
          try {
            val state = proposal.state

            // Determine if we should process this proposal based on state and config
            val shouldProcess = state.toLowerCase match {
              case DetectorSnapshotGov.STATE_ACTIVE => trackActive
              case DetectorSnapshotGov.STATE_CLOSED => trackClosed
              case DetectorSnapshotGov.STATE_PENDING => trackPending
              case _ => false
            }

            log.info(s"${rx.getExtId()}: Proposal ${proposal.id} (state=$state, tracking=$shouldProcess)")

            if (!shouldProcess) {
              log.debug(s"${rx.getExtId()}: Proposal ${proposal.id}: SKIP (state=$state, tracking disabled)")
              None
            } else {
              val distribution = DetectorSnapshotGov.calculateVoteDistribution(proposal)
              val totalVotes = proposal.scores_total.getOrElse(0.0)
              val quorum = proposal.quorum.getOrElse(0.0)
              val quorumMet = totalVotes >= quorum
              val voteCount = proposal.votes.getOrElse(0)

              val winningChoice = DetectorSnapshotGov.getWinningChoice(proposal)
              val (winningName, winningScore) = winningChoice.getOrElse(("N/A", 0.0))

              val winningPercent = if (totalVotes > 0) (winningScore / totalVotes) * 100 else 0.0

              // Determine severity based on state and conditions
              val (severity, alertReason) = state match {
                case DetectorSnapshotGov.STATE_CLOSED =>
                  if (!quorumMet) {
                    (Severity.HIGH, "Quorum NOT reached")
                  } else {
                    (Severity.MEDIUM, "Quorum reached")
                  }

                case DetectorSnapshotGov.STATE_ACTIVE =>
                  (Severity.INFO, "Voting in progress")

                case DetectorSnapshotGov.STATE_PENDING =>
                  (Severity.LOW, "Not started")

                case _ =>
                  (Severity.INFO, "$state")
              }

              // Generate deterministic event ID based on state
              val eid0 = state match {
                case DetectorSnapshotGov.STATE_CLOSED =>
                  // Deterministic: did + proposalId + extId only (final state)
                  s"${did}-${proposal.id}-${rx.getExtId()}"

                case DetectorSnapshotGov.STATE_ACTIVE =>
                  // Changes with votes: did + proposalId + extId + totalVotes
                  s"${did}-${proposal.id}-${rx.getExtId()}-${totalVotes.toLong}"

                case _ =>
                  // Other states: did + proposalId + extId + state
                  s"${did}-${proposal.id}-${rx.getExtId()}-${state}"
              }

              val eid = Util.sha256(eid0)

              log.info(s"${rx.getExtId()}: Proposal ${proposal.id} [$state]: ${proposal.title} - Winning: $winningName (${winningPercent.toInt}%) - Total VP: ${totalVotes.toLong} - Votes: $voteCount - Severity: $severity - $alertReason")

              // Build choice distribution metadata
              val choicesMeta = distribution.zipWithIndex.map { case ((choice, score), idx) =>
                val percent = if (totalVotes > 0) (score / totalVotes) * 100 else 0.0
                s"choice_${idx + 1}" -> s"$choice: ${score.toLong} VP (${percent.toInt}%)"
              }.toMap

              Some(EventUtil.createEvent(
                did = did,
                tx = None,
                monitoredAddr = rx.getAddr(),
                conf = Some(rx.getConf()),
                meta = Map(
                  "desc" -> desc,
                  "id" -> proposal.id,
                  "title" -> proposal.title,
                  "author" -> proposal.author,
                  "state" -> (state.take(1).toUpperCase + state.drop(1)),
                  "space" -> proposal.space.id,
                  "space_name" -> proposal.space.name,
                  "start" -> DetectorSnapshotGov.epochSecondToUtcIso(proposal.start),
                  "end" -> DetectorSnapshotGov.epochSecondToUtcIso(proposal.end),
                  "snapshot" -> proposal.snapshot,
                  "winning_choice" -> winningName,
                  "winning_score" -> winningScore.toString,
                  "winning_percentage" -> f"$winningPercent%.2f",
                  "total_votes" -> totalVotes.toString,
                  "vote_count" -> voteCount.toString,
                  "quorum" -> quorum.toString,
                  "quorum_met" -> quorumMet.toString,
                  "choices_count" -> proposal.choices.length.toString,
                  "reason" -> alertReason,
                  "link" -> s"https://snapshot.org/#/${proposal.space.id}/proposal/${proposal.id}",
                  "tx_hash" -> proposal.id
                ) ++ choicesMeta,
                sev = Some(severity),
                eid0 = Some(eid),
                detectorTs = (proposal.start * 1000L).toString
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
        error(s"Failed to fetch Snapshot governance proposals", Some(e.getMessage))
        Seq.empty
    }
  }
}
