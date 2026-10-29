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
      <PREFIX>ACCESS_KEY, <PREFIX>ACCESS_SECRET, <PREFIX>SESSION_TOKEN, <PREFIX>REGION, <PREFIX>ACCOUNT
  - Credential components can be url-encoded (%2F, %2B ...) 
  - Parameters (`?param=value&param=value`) are available as `params`. 
    Well-known parameters: `region`, `account` (override <PREFIX>REGION, <PREFIX>ACCOUNT), `ssl` (default `true`, used with custom host)
    Credentials refresh (AWS): `profile`, `profile_file`, `role_arn`, `external_id`, `role_session`, `role_duration`
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
  def accessKey: Option[String] = _key.orElse(envPair.map(_.key)).orElse(envFirst(envNames("ACCESS_KEY")))
  def accessSecret: Option[String] = _secret.orElse(envPair.map(_.secret)).orElse(envFirst(envNames("ACCESS_SECRET")))
  def sessionToken: Option[String] = _token.orElse(envFirst(envNames("SESSION_TOKEN")))

  // URI parameter first, then environment: AWS_REGION (AWS_DEFAULT_REGION), GCP_REGION, AZURE_REGION
  def region: Option[String] = _params.get("region").filter(_.nonEmpty).orElse(envFirst(envNames("REGION")))

  // Cloud account (AWS account id, GCP account/project, Azure subscription): `account=` parameter, then environment: AWS_ACCOUNT, GCP_ACCOUNT, AZURE_ACCOUNT
  def account: Option[String] = _params.get("account").filter(_.nonEmpty).orElse(envFirst(envNames("ACCOUNT")))

  def hasCredentials: Boolean = accessKey.isDefined && accessSecret.isDefined

  /** Credentials explicitly specified in the URI itself (not from environment) */
  def hasUriCredentials: Boolean = _key.isDefined && _secret.isDefined

  /** Session token explicitly specified in the URI (never mixed with key/secret from environment) */
  def uriSessionToken: Option[String] = _token

  /** Credentials from environment only (static, never refreshed by the process) */
  def hasEnvCredentials: Boolean = envPair.isDefined

  /** Names of environment variables which are used for key and secret (for logs). Never values */
  def envCredentialsNames: Option[String] = envPair.map(p => s"${p.keyName},${p.secretName}")

  /** Named profile from shared credentials file (refreshable): ?profile=dev */
  def profile: Option[String] = _params.get("profile").filter(_.nonEmpty)

  /** Role to assume (credentials are refreshed automatically): ?role_arn=arn:aws:iam::123456789012:role/Name */
  def roleArn: Option[String] = _params.get("role_arn").filter(_.nonEmpty)

  /** Endpoint URL if custom host is specified in URI */
  def endpoint: Option[String] = _host.map { h =>
    if (h.startsWith("http://") || h.startsWith("https://")) h
    else if (_params.get("ssl").exists(_.equalsIgnoreCase("false"))) s"http://${h}"
    else s"https://${h}"
  }

  def param(name: String, default: String = ""): String = _params.get(name).getOrElse(default)

  def paramList(name: String): Seq[String] =
    _params.get(name).map(_.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq).getOrElse(Seq.empty)

  private case class EnvPair(keyName: String, secretName: String, key: String, secret: String)

  /**
   * Key and secret from environment are always taken as a PAIR from the same naming scheme (never key from one scheme and secret from another):
   *   `AWS_ACCESS_KEY` + `AWS_ACCESS_SECRET` (cloud prefix) or `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY` (standard AWS)
   * 
   * Both schemes may be present at once (e.g. dummy `AWS_ACCESS_KEY=test` for mock and real keys from `aws configure export-credentials`).
   * Session token (AWS_SESSION_TOKEN) is issued only together with the standard AWS keys: if it is defined, standard keys have priority.
   * Otherwise cloud prefix has priority
   */
  private lazy val envPair: Option[EnvPair] = {
    val prefixed = (prefix + "ACCESS_KEY", prefix + "ACCESS_SECRET")
    val standard = if (_cloud == "aws") Seq(("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")) else Seq.empty
    val hasToken = envFirst(Seq(prefix + "SESSION_TOKEN")).isDefined
    val schemes = if (hasToken) standard :+ prefixed else prefixed +: standard

    schemes.iterator.flatMap { case (kn, sn) =>
      for { k <- envFirst(Seq(kn)); s <- envFirst(Seq(sn)) } yield EnvPair(kn, sn, k, s)
    }.nextOption()
  }

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
    // only credentials specified in URI itself (those from environment are not part of URI)
    val cred = _key.map { k =>
      val sec = if (_secret.isDefined) ":***" else ""
      val tok = if (_token.isDefined) ":***" else ""
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
