package io.syspulse.ext.sentinel.feeds

import io.syspulse.skel.util.Util

/*
  meltwater://{id},{id},...?apiKey=KEY&window=24h&...

  the apiKey is ALWAYS an option
  (?apiKey=...), never positioned after the prefix. The path part after `meltwater://`
  carries ONLY the search id(s). apiKey resolves from the ?apiKey= option (with ${ENV}
  substitution), falling back to the MELTWATER_API_KEY environment variable.
*/
case class MeltwaterURI(uri: String) {
  val PREFIX = "meltwater"

  private val (_ids: Seq[String], _ops: Map[String, String]) = parse(uri)

  def ids: Seq[String] = _ids
  def ops: Map[String, String] = _ops

  /** ?apiKey=... (with ${ENV} substitution) or the MELTWATER_API_KEY env var. */
  def apiKey: String =
    _ops.get("apiKey").map(k => Util.replaceEnvVar(k)).filter(_.nonEmpty)
      .getOrElse(sys.env.getOrElse("MELTWATER_API_KEY", ""))

  private def parse(uri: String): (Seq[String], Map[String, String]) = {
    // Strip the meltwater:// prefix (leave anything else untouched).
    val body = uri.split("://").toList match {
      case PREFIX :: rest => rest.headOption.getOrElse("")
      case _              => uri
    }

    // Split the id path from the ?k=v&k=v options.
    val (id, ops) = body.split("[\\?&]").toList match {
      case path :: Nil => (path, Map.empty[String, String])
      case path :: opts =>
        val vars = opts.flatMap(_.split("=").toList match {
          case k :: v :: Nil => Some(k -> v)
          case _             => None
        }).toMap
        (path, vars)
      case _ => ("", Map.empty[String, String])
    }

    val ids = id.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    (ids, ops)
  }
}
