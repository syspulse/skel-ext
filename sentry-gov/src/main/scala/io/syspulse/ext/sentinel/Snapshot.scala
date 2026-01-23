package io.syspulse.ext.sentinel

import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.Logger
import spray.json._
import io.syspulse.skel.service.JsonCommon

case class SnapshotProposal(
  id: String,
  title: String,
  body: String,
  choices: Seq[String],
  start: Long,
  end: Long,
  snapshot: String,
  state: String,
  author: String,
  space: SnapshotSpace,
  scores: Option[Seq[Double]],
  scores_total: Option[Double],
  quorum: Option[Double],
  votes: Option[Int]
)

case class SnapshotSpace(
  id: String,
  name: String
)

case class SnapshotVote(
  id: String,
  voter: String,
  vp: Double,
  vp_by_strategy: Option[Seq[Double]],
  vp_state: Option[String],
  created: Long,
  choice: JsValue,
  space: SnapshotSpace
)

case class SnapshotProposalsResponse(
  proposals: Seq[SnapshotProposal]
)

case class SnapshotVotesResponse(
  votes: Seq[SnapshotVote]
)

object SnapshotJsonProtocol extends DefaultJsonProtocol {
  implicit val spaceFormat: RootJsonFormat[SnapshotSpace] = jsonFormat2(SnapshotSpace)

  implicit val proposalFormat: RootJsonFormat[SnapshotProposal] = new RootJsonFormat[SnapshotProposal] {
    def write(p: SnapshotProposal): JsValue = JsObject(
      "id" -> JsString(p.id),
      "title" -> JsString(p.title),
      "body" -> JsString(p.body),
      "choices" -> p.choices.toJson,
      "start" -> JsNumber(p.start),
      "end" -> JsNumber(p.end),
      "snapshot" -> JsString(p.snapshot),
      "state" -> JsString(p.state),
      "author" -> JsString(p.author),
      "space" -> p.space.toJson,
      "scores" -> p.scores.toJson,
      "scores_total" -> p.scores_total.toJson,
      "quorum" -> p.quorum.toJson,
      "votes" -> p.votes.toJson
    )

    def read(value: JsValue): SnapshotProposal = {
      val fields = value.asJsObject.fields

      // Handle snapshot field which can be either String or Number
      val snapshotStr = fields.get("snapshot").map {
        case JsString(s) => s
        case JsNumber(n) => n.toString
        case _ => ""
      }.getOrElse("")

      SnapshotProposal(
        id = fields("id").convertTo[String],
        title = fields("title").convertTo[String],
        body = fields.get("body").map(_.convertTo[String]).getOrElse(""),
        choices = fields.get("choices").map(_.convertTo[Seq[String]]).getOrElse(Seq.empty),
        start = fields("start").convertTo[Long],
        end = fields("end").convertTo[Long],
        snapshot = snapshotStr,
        state = fields("state").convertTo[String],
        author = fields("author").convertTo[String],
        space = fields("space").convertTo[SnapshotSpace],
        scores = fields.get("scores").flatMap(_.convertTo[Option[Seq[Double]]]),
        scores_total = fields.get("scores_total").flatMap(_.convertTo[Option[Double]]),
        quorum = fields.get("quorum").flatMap(_.convertTo[Option[Double]]),
        votes = fields.get("votes").flatMap(_.convertTo[Option[Int]])
      )
    }
  }

  implicit val voteFormat: RootJsonFormat[SnapshotVote] = new RootJsonFormat[SnapshotVote] {
    def write(v: SnapshotVote): JsValue = JsObject(
      "id" -> JsString(v.id),
      "voter" -> JsString(v.voter),
      "vp" -> JsNumber(v.vp),
      "vp_by_strategy" -> v.vp_by_strategy.toJson,
      "vp_state" -> v.vp_state.toJson,
      "created" -> JsNumber(v.created),
      "choice" -> v.choice,
      "space" -> v.space.toJson
    )

    def read(value: JsValue): SnapshotVote = {
      val fields = value.asJsObject.fields
      SnapshotVote(
        id = fields("id").convertTo[String],
        voter = fields("voter").convertTo[String],
        vp = fields.get("vp").map(_.convertTo[Double]).getOrElse(0.0),
        vp_by_strategy = fields.get("vp_by_strategy").flatMap(_.convertTo[Option[Seq[Double]]]),
        vp_state = fields.get("vp_state").flatMap(_.convertTo[Option[String]]),
        created = fields("created").convertTo[Long],
        choice = fields("choice"),
        space = fields("space").convertTo[SnapshotSpace]
      )
    }
  }
}

class Snapshot(apiKey: Option[String] = None) {
  private val log = Logger(getClass.getName)

  private val BASE_URL = "https://hub.snapshot.org/graphql"

  private def graphqlQuery(query: String): Try[JsValue] = Try {
    val payload = ujson.Obj("query" -> query)

    val headers = Map("Content-Type" -> "application/json") ++
      apiKey.map(key => Map("x-api-key" -> key)).getOrElse(Map.empty)

    val response = requests.post(
      BASE_URL,
      data = ujson.write(payload),
      headers = headers,
      readTimeout = 30000,
      connectTimeout = 10000
    )

    if (response.statusCode != 200) {
      throw new Exception(s"Snapshot API error (${response.statusCode}): ${response.text()}")
    }

    response.text().parseJson
  }

  def fetchRecentProposals(space: String, count: Int = 5, state: Option[String] = None): Try[Seq[SnapshotProposal]] = {
    val stateFilter = state.map(s => s"""state: "$s",""").getOrElse("")

    val query = s"""
    {
      proposals(
        first: $count,
        skip: 0,
        where: {
          space_in: ["$space"],
          $stateFilter
        },
        orderBy: "created",
        orderDirection: desc
      ) {
        id
        title
        body
        choices
        start
        end
        snapshot
        state
        author
        space {
          id
          name
        }
        scores
        scores_total
        quorum
        votes
      }
    }
    """

    graphqlQuery(query).flatMap { result =>
      Try {
        import SnapshotJsonProtocol._
        val data = result.asJsObject.fields("data").asJsObject
        data.fields("proposals").convertTo[Seq[SnapshotProposal]]
      }
    }
  }

  def fetchSpecificProposals(space: String, proposalIds: Seq[String]): Try[Seq[SnapshotProposal]] = {
    val idsFilter = proposalIds.map(id => s""""$id"""").mkString("[", ",", "]")

    val query = s"""
    {
      proposals(
        where: {
          space: "$space",
          id_in: $idsFilter
        }
      ) {
        id
        title
        body
        choices
        start
        end
        snapshot
        state
        author
        space {
          id
          name
        }
        scores
        scores_total
        quorum
        votes
      }
    }
    """

    graphqlQuery(query).flatMap { result =>
      Try {
        import SnapshotJsonProtocol._
        val data = result.asJsObject.fields("data").asJsObject
        data.fields("proposals").convertTo[Seq[SnapshotProposal]]
      }
    }
  }

  def fetchVotesForProposal(proposalId: String, limit: Int = 100): Try[Seq[SnapshotVote]] = {
    val query = s"""
    {
      votes(
        first: $limit,
        skip: 0,
        where: {
          proposal: "$proposalId"
        },
        orderBy: "vp",
        orderDirection: desc
      ) {
        id
        voter
        vp
        vp_by_strategy
        vp_state
        created
        choice
        space {
          id
          name
        }
      }
    }
    """

    graphqlQuery(query).flatMap { result =>
      Try {
        import SnapshotJsonProtocol._
        val data = result.asJsObject.fields("data").asJsObject
        data.fields("votes").convertTo[Seq[SnapshotVote]]
      }
    }
  }

  def parseChoiceValue(choice: JsValue, choices: Seq[String]): String = {
    choice match {
      case JsNumber(n) =>
        val index = n.toInt - 1
        if (index >= 0 && index < choices.length) choices(index) else s"Choice $n"
      case JsArray(arr) =>
        arr.map(v => parseChoiceValue(v, choices)).mkString(", ")
      case JsObject(obj) =>
        obj.map { case (k, v) => s"$k: ${v.toString}" }.mkString(", ")
      case other =>
        other.toString
    }
  }
}
