package io.syspulse.ext.sentinel.cloud.auth

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

import io.syspulse.skel.util.Util

/*
  Auth URI: how to authenticate in the Cloud (`auth` attribute of the Detector config).
  It has a preference over credentials in Cloud URI (`uri`): if `auth` is specified, credentials (and credentials related parameters) of `uri` are ignored.
  
    <scheme>://<body>?param=value&param=value

  Schemes:

    secret://<ACCESS_KEY_ID>:<SECRET_ACCESS_KEY>[:SESSION_TOKEN]
      Static credentials. SESSION_TOKEN is optional (if it is not specified, it is requested from the Cloud and refreshed: AWS STS)
      Use `{VAR}` or `${VAR}` to take values from environment: secret://{AWS_ACCESS_KEY}:{AWS_ACCESS_SECRET}
      Never put real credentials into configuration files !

    env://[$ACCESS_KEY_ID]:[$SECRET_ACCESS_KEY]:[$SESSION_TOKEN]
      Names of the environment variables (`$` is optional). Any name can be omitted: default name of the Cloud is used (AWS_ACCESS_KEY, AWS_ACCESS_SECRET, AWS_SESSION_TOKEN).
        env://                              all default
        env://$MY_KEY:$MY_SECRET            custom names, no SESSION_TOKEN (it is never taken from a default variable if key or secret names are custom)
        env://$MY_KEY:$MY_SECRET:$MY_TOKEN
        env://::$MY_TOKEN                   default key and secret, custom SESSION_TOKEN variable

    profile://[file]
      Profile from the shared credentials file. `file` is optional (default: ~/.aws/credentials). Profile name: `profile` parameter, AWS_PROFILE or `default`.
      Profile may use `credential_process` (SSO, Vault, ...): credentials are refreshed automatically
        profile://?profile=sentry
        profile:///etc/sentry/credentials?profile=sentry

    role://
      IAM role of the environment: credentials of the role (task, pod, instance profile, web identity) are taken from the Cloud and refreshed automatically.
      `arn` parameter: assume another role with these credentials
        role://
        role://?arn=arn:aws:iam::123456789012:role/Sentry&external_id=abc

  Common parameters (any scheme):
    arn (role_arn)               - assume the role (credentials are refreshed automatically)
    external_id                  - external id for assuming the role
    session (role_session)       - session name of the assumed role (default: sentry-cloud)
    duration (role_duration)     - session duration of the assumed role, seconds (default: 3600)
    sts                          - `false`: do not request SESSION_TOKEN for keys without it

  AuthURI only parses the string. Use `Auth.resolve` to get the credentials source.
*/
case class AuthURI(uri: String, env: Map[String, String] = sys.env) {

  private val (_scheme: String, _body: String, _params: Map[String, String]) = parse(uri)

  /** Scheme in lower case: secret, env, profile, role (empty if the URI is invalid) */
  def scheme: String = _scheme

  /** Everything between `://` and `?` as it is written */
  def body: String = _body

  def params: Map[String, String] = _params

  def param(name: String, default: String = ""): String = _params.get(name).getOrElse(default)

  /** First of the parameters (aliases) which is specified */
  def paramOf(names: String*): Option[String] = names.iterator.flatMap(n => _params.get(n)).find(_.nonEmpty)

  def isSupported: Boolean = AuthURI.SCHEMES.contains(_scheme)

  // ---- secret://<ACCESS_KEY_ID>:<SECRET_ACCESS_KEY>[:SESSION_TOKEN]
  private lazy val secretParts: Seq[Option[String]] = {
    val p = _body.split(":", 3).toSeq.map(decode).map(_.trim).map(s => Option(s).filter(_.nonEmpty))
    p ++ Seq.fill(3 - p.size)(None)
  }

  def keyId: Option[String] = if (_scheme == AuthURI.SECRET) secretParts(0) else None
  def secretKey: Option[String] = if (_scheme == AuthURI.SECRET) secretParts(1) else None
  def sessionToken: Option[String] = if (_scheme == AuthURI.SECRET) secretParts(2) else None

  // ---- env://[$ACCESS_KEY_ID]:[$SECRET_ACCESS_KEY]:[$SESSION_TOKEN]
  /** Names of the environment variables (key, secret, token). None: default name of the Cloud */
  lazy val envNames: (Option[String], Option[String], Option[String]) = {
    def name(s: String): Option[String] = {
      val n = s.trim.stripPrefix("$").stripPrefix("{").stripSuffix("}").trim
      Option(n).filter(_.nonEmpty)
    }
    val p = _body.split(":", 3).toSeq.map(name)
    val pp = p ++ Seq.fill(3 - p.size)(None)
    (pp(0), pp(1), pp(2))
  }

  // ---- profile://[file]
  /** Credentials file (profile://) */
  def file: Option[String] = if (_scheme == AuthURI.PROFILE) {
    Option(decode(_body).trim).filter(_.nonEmpty).map { f =>
      if (f == "~") sys.props.getOrElse("user.home", f)
      else if (f.startsWith("~/")) sys.props.getOrElse("user.home", "~") + f.substring(1)
      else f
    }
  } else None

  def profile: Option[String] = _params.get("profile").filter(_.nonEmpty)

  // ---- role
  def roleArn: Option[String] = paramOf("arn", "role_arn")
  def externalId: Option[String] = paramOf("external_id")
  def session: Option[String] = paramOf("session", "role_session")
  def duration: Option[String] = paramOf("duration", "role_duration")

  /** `sts=false` disables requesting of SESSION_TOKEN */
  def stsDisabled: Boolean = _params.get("sts").exists(_.equalsIgnoreCase("false"))

  // never leak credentials into logs
  override def toString: String = {
    val b = _scheme match {
      case AuthURI.SECRET => 
        val k = keyId.map(k => s"${k.take(4)}***").getOrElse("")
        val s = if (secretKey.isDefined) ":***" else ""
        val t = if (sessionToken.isDefined) ":***" else ""
        s"${k}${s}${t}"
      case _ => _body
    }
    val q = if (_params.isEmpty) "" else "?" + _params.map { case (k, v) => s"${k}=${v}" }.mkString("&")
    s"AuthURI(${_scheme}://${b}${q})"
  }

  // `{VAR}` and `${VAR}` are expanded from environment
  private def decode(s: String): String = {
    val s1 = if (s.contains("{")) Util.replaceEnvVar(s, env) else s
    try { URLDecoder.decode(s1.replace("+", "%2B"), StandardCharsets.UTF_8.name()) } catch { case _: Exception => s1 }
  }

  private def parse(uri: String): (String, String, Map[String, String]) = {
    val i = uri.indexOf("://")
    if (i < 0) return ("", "", Map.empty)

    val scheme = uri.substring(0, i).trim.toLowerCase
    val rest = uri.substring(i + 3)

    val (body, query) = rest.indexOf('?') match {
      case -1 => (rest, "")
      case q => (rest.substring(0, q), rest.substring(q + 1))
    }

    val params = query.split("&").iterator.map(_.trim).filter(_.nonEmpty).flatMap { kv =>
      kv.split("=", 2).toList match {
        case k :: v :: Nil => Some(decode(k) -> decode(v))
        case k :: Nil => Some(decode(k) -> "")
        case _ => None
      }
    }.toMap

    (scheme, body, params)
  }
}

object AuthURI {
  val SECRET = "secret"
  val ENV = "env"
  val PROFILE = "profile"
  val ROLE = "role"

  val SCHEMES = Seq(SECRET, ENV, PROFILE, ROLE)
}
