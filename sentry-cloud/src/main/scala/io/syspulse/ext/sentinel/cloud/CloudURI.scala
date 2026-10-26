package io.syspulse.ext.sentinel.cloud

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

import io.syspulse.skel.util.Util

/*
  Cloud URI:

    <cloud>://ACCESS_KEY:ACCESS_SECRET[:SESSION_TOKEN]@<cloud_host>?param=value&param=value

  <cloud> is one of: aws, gcp, azure

  Examples:
    aws://                                                    - everything from environment (AWS_ACCESS_KEY, AWS_ACCESS_SECRET, AWS_SESSION_TOKEN, AWS_REGION)
    aws://?region=eu-west-1                                   - credentials from environment, region from URI
    aws://AKIA...:secret@?region=eu-west-1                    - static credentials
    aws://AKIA...:secret:token@?region=eu-west-1              - temporary credentials (session token)
    aws://{AWS_ACCESS_KEY}:{AWS_ACCESS_SECRET}@?region=us-east-1  - credentials from environment (`{VAR}` or `${VAR}`)
    aws://localhost:4566?region=us-east-1&ssl=false           - custom endpoint (LocalStack)
    gcp://?project=my-project
    azure://?subscription=0000-0000

  Rules:
  - '@' is REQUIRED when credentials are specified in URI (otherwise ACCESS_KEY:ACCESS_SECRET cannot be distinguished from host:port)
  - Credentials which are not specified in URI are taken from environment variables. 
    Prefix of the variable is defined by the cloud: aws:// -> AWS_, gcp:// -> GCP_, azure:// -> AZURE_
      <PREFIX>ACCESS_KEY, <PREFIX>ACCESS_SECRET, <PREFIX>SESSION_TOKEN, <PREFIX>REGION
  - Credential components can be url-encoded (%2F, %2B ...) 
  - Parameters (`?param=value&param=value`) are available as `params`. 
    Well-known parameters: `region`, `ssl` (default `true`, used with custom host)
*/
case class CloudURI(uri: String, env: Map[String, String] = sys.env) {

  private val (_cloud: String, _key: Option[String], _secret: Option[String], _token: Option[String], _host: Option[String], _params: Map[String, String]) = parse(uri)

  /** Cloud name in lower case: aws, gcp, azure (empty if URI is invalid) */
  def cloud: String = _cloud
  /** Environment variables prefix: aws -> AWS_ */
  def prefix: String = s"${_cloud.toUpperCase}_"

  def host: Option[String] = _host
  def params: Map[String, String] = _params

  // credentials: URI first, then environment
  def accessKey: Option[String] = _key.orElse(envFirst(envNames("ACCESS_KEY")))
  def accessSecret: Option[String] = _secret.orElse(envFirst(envNames("ACCESS_SECRET")))
  def sessionToken: Option[String] = _token.orElse(envFirst(envNames("SESSION_TOKEN")))

  def region: Option[String] = _params.get("region").filter(_.nonEmpty).orElse(envFirst(envNames("REGION")))

  def hasCredentials: Boolean = accessKey.isDefined && accessSecret.isDefined

  /** Endpoint URL if custom host is specified in URI */
  def endpoint: Option[String] = _host.map { h =>
    if (h.startsWith("http://") || h.startsWith("https://")) h
    else if (_params.get("ssl").exists(_.equalsIgnoreCase("false"))) s"http://${h}"
    else s"https://${h}"
  }

  def param(name: String, default: String = ""): String = _params.get(name).getOrElse(default)

  def paramList(name: String): Seq[String] =
    _params.get(name).map(_.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq).getOrElse(Seq.empty)

  // Standard AWS variable names are also supported for convenience
  private def envNames(suffix: String): Seq[String] = {
    val std = (_cloud, suffix) match {
      case ("aws", "ACCESS_KEY") => Seq("AWS_ACCESS_KEY_ID")
      case ("aws", "ACCESS_SECRET") => Seq("AWS_SECRET_ACCESS_KEY")
      case ("aws", "REGION") => Seq("AWS_DEFAULT_REGION")
      case _ => Seq.empty
    }
    (prefix + suffix) +: std
  }

  private def envFirst(names: Seq[String]): Option[String] =
    names.iterator.flatMap(n => env.get(n)).find(_.nonEmpty)

  // never leak credentials into logs
  override def toString: String = {
    val cred = accessKey.map { k =>
      val sec = if (accessSecret.isDefined) ":***" else ""
      val tok = if (sessionToken.isDefined) ":***" else ""
      s"${k.take(4)}***${sec}${tok}@"
    }.getOrElse("")
    val q = if (_params.isEmpty) "" else "?" + _params.map { case (k, v) => s"${k}=${v}" }.mkString("&")
    s"CloudURI(${_cloud}://${cred}${_host.getOrElse("")}${q})"
  }

  private def decode(s: String): String = {
    // support env expansion for credentials: {AWS_ACCESS_KEY} or ${AWS_ACCESS_KEY}
    val s1 = if (s.contains("{")) Util.replaceEnvVar(s, env) else s
    try { URLDecoder.decode(s1.replace("+", "%2B"), StandardCharsets.UTF_8.name()) } catch { case _: Exception => s1 }
  }

  private def parse(uri: String): (String, Option[String], Option[String], Option[String], Option[String], Map[String, String]) = {
    val i = uri.indexOf("://")
    if (i < 0) return ("", None, None, None, None, Map.empty)

    val cloud = uri.substring(0, i).toLowerCase
    val body = uri.substring(i + 3)

    val (main, query) = body.indexOf('?') match {
      case -1 => (body, "")
      case q => (body.substring(0, q), body.substring(q + 1))
    }

    // credentials may contain '/', '+', '=' : split by the LAST '@'
    val (cred, host) = main.lastIndexOf('@') match {
      case -1 => ("", main)
      case a => (main.substring(0, a), main.substring(a + 1))
    }

    def opt(s: String): Option[String] = Option(s).map(decode).map(_.trim).filter(_.nonEmpty)

    val (key, secret, token) = cred.split(":", 3).toList match {
      case k :: s :: t :: Nil => (opt(k), opt(s), opt(t))
      case k :: s :: Nil => (opt(k), opt(s), None)
      case k :: Nil => (opt(k), None, None)
      case _ => (None, None, None)
    }

    val params = query.split("&").iterator.map(_.trim).filter(_.nonEmpty).flatMap { kv =>
      kv.split("=", 2).toList match {
        case k :: v :: Nil => Some(decode(k) -> decode(v))
        case k :: Nil => Some(decode(k) -> "")
        case _ => None
      }
    }.toMap

    (cloud, key, secret, token, Option(host).map(_.trim).filter(_.nonEmpty), params)
  }
}
