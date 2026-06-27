package io.syspulse.ext.sentinel.feeds

import scala.util.{Failure, Success, Try}

object FeedId {

  /** Build a stable id from a feed-provided id/link/title.
    *
    * Requirement: if the value starts with http(s), do NOT keep full URL as id.
    * Instead use a "slug" (last meaningful path segment) so id is not `https://host/...`.
    */
  def stableId(raw: String): String = {
    val s = Option(raw).getOrElse("").trim
    if (s.isEmpty) return ""

    if (s.startsWith("http://") || s.startsWith("https://")) slugFromUrl(s)
    else s
  }

  private def slugFromUrl(url: String): String = {
    def lastSegmentFromString(s: String): String = {
      val noFrag = s.takeWhile(_ != '#')
      val noQuery = noFrag.takeWhile(_ != '?')
      val noTrail = noQuery.reverse.dropWhile(_ == '/').reverse
      noTrail.split("/").reverseIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
    }

    Try(new java.net.URI(url)) match {
      case Success(uri) =>
        val path = Option(uri.getPath).getOrElse("")
        val seg =
          path.reverse
            .dropWhile(_ == '/')
            .reverse
            .split("/")
            .reverseIterator
            .map(_.trim)
            .find(_.nonEmpty)
            .getOrElse("")

        if (seg.nonEmpty) {
          seg
        } else {
          // Some feeds use URLs like https://site.com/?p=123 as "guid".
          // If there's no meaningful path segment, use a stable query-derived slug.
          val query = Option(uri.getQuery).getOrElse("").trim

          def queryParam(name: String): Option[String] =
            query.split("&").toSeq
              .flatMap(_.split("=", 2) match {
                case Array(k, v) if k == name && v.nonEmpty => Some(v)
                case _ => None
              })
              .headOption

          queryParam("p")
            .orElse(queryParam("id"))
            .orElse(queryParam("post"))
            .filter(_.nonEmpty)
            .getOrElse {
              if (query.nonEmpty) query
              else Option(uri.getHost).getOrElse(lastSegmentFromString(url))
            }
        }

      case Failure(_) =>
        lastSegmentFromString(url)
    }
  }
}

