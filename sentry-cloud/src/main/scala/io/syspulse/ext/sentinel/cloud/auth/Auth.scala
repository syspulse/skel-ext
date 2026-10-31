package io.syspulse.ext.sentinel.cloud.auth

import scala.util.Try

import io.syspulse.ext.sentinel.cloud.CloudURI

/**
 * Source of Cloud credentials. It is resolved from `auth` (AuthURI) or, if `auth` is not specified, from Cloud URI (`uri`).
 * Resolving is pure: no network access, no Cloud SDK.
 */
sealed trait Auth {
  /** Description for logs and tests. Never contains secrets */
  def name: String

  /** Source of base credentials (role wrapper is skipped) */
  def base: Auth = this

  /** Static SESSION_TOKEN is used: credentials expire and cannot be refreshed */
  def isStatic: Boolean = false
}

object Auth {
  val DEF_ROLE_SESSION = "sentry-cloud"
  val DEF_ROLE_DURATION = 3600                // seconds. Credentials are refreshed automatically before expiration

  val SOURCE_URI = "uri"                      // keys in Cloud URI
  val SOURCE_SECRET = "secret"                // keys in secret://
  val SOURCE_ENV = "env"                      // keys from environment

  /**
   * Static keys.
   * @param token        SESSION_TOKEN. Such credentials EXPIRE and are NOT refreshed
   * @param requestToken no SESSION_TOKEN: request it from the Cloud (AWS STS GetSessionToken), it is refreshed automatically
   * @param detail       description of the source (names of environment variables)
   */
  case class Static(key: String, secret: String, token: Option[String], source: String, requestToken: Boolean, detail: String = "") extends Auth {
    def name: String = source + (if (requestToken) "+sts" else "")
    override def isStatic: Boolean = token.isDefined
    override def toString = s"Static(${source}:${key.take(4)}***${if (token.isDefined) ":***" else ""})"
  }

  /** Profile of the shared credentials file (file: default location if empty) */
  case class Profile(profile: String, file: Option[String]) extends Auth {
    def name: String = "profile"
  }

  /** Default credentials chain of the Cloud (environment, profile, web identity, container, instance profile, ...) */
  case class Default(label: String = "default") extends Auth {
    def name: String = label
  }

  /** Assume the role with credentials of `underlying` */
  case class Role(arn: String, externalId: Option[String], session: String, duration: Int, underlying: Auth) extends Auth {
    def name: String = s"role(${underlying.name})"
    override def base: Auth = underlying.base
    override def isStatic: Boolean = underlying.isStatic
  }

  /**
   * Resolve credentials source.
   * 
   * `auth` has a preference: if it is specified, credentials of `uri` (keys, `profile`, `role_arn`, ...) are ignored. 
   * Throws IllegalArgumentException if `auth` is invalid
   */
  def resolve(uri: CloudURI, auth: Option[AuthURI]): Auth = auth match {
    case Some(a) => fromAuth(uri, a)
    case None => fromUri(uri)
  }

  private def duration(s: Option[String]): Int = s.flatMap(d => Try(d.trim.toInt).toOption).getOrElse(DEF_ROLE_DURATION)

  private def requestToken(token: Option[String], hasRole: Boolean, stsDisabled: Boolean): Boolean =
    token.isEmpty && !hasRole && !stsDisabled

  // ---- Cloud URI: aws://KEY:SECRET[:TOKEN]@?profile=&profile_file=&role_arn=&external_id=&role_session=&role_duration=&sts=
  // Priority: keys in URI > profile > keys from environment > default chain 
  private def fromUri(uri: CloudURI): Auth = {
    val hasRole = uri.roleArn.isDefined
    val stsDisabled = uri.param("sts", "true").equalsIgnoreCase("false")

    val base: Auth =
      if (uri.hasUriCredentials) {
        val token = uri.uriSessionToken
        Static(uri.accessKey.get, uri.accessSecret.get, token, SOURCE_URI, requestToken(token, hasRole, stsDisabled))
      } else if (uri.profile.isDefined) {
        Profile(uri.profile.get, Option(uri.param("profile_file")).filter(_.nonEmpty))
      } else if (uri.hasEnvCredentials) {
        val token = uri.sessionToken
        Static(uri.accessKey.get, uri.accessSecret.get, token, SOURCE_ENV, requestToken(token, hasRole, stsDisabled), uri.envCredentialsNames.getOrElse(""))
      } else Default()

    uri.roleArn match {
      case Some(arn) => Role(arn, Option(uri.param("external_id")).filter(_.nonEmpty), uri.param("role_session", DEF_ROLE_SESSION), duration(uri.params.get("role_duration")), base)
      case None => base
    }
  }

  // ---- AuthURI 
  private def fromAuth(uri: CloudURI, a: AuthURI): Auth = {
    if (!a.isSupported) {
      throw new IllegalArgumentException(
        if (a.scheme.isEmpty) s"invalid auth uri: '${a}' (expected: ${AuthURI.SCHEMES.map(_ + "://").mkString(", ")})"
        else s"unsupported auth uri: '${a.scheme}://' (expected: ${AuthURI.SCHEMES.map(_ + "://").mkString(", ")})")
    }

    val hasRole = a.roleArn.isDefined

    val base: Auth = a.scheme match {
      case AuthURI.SECRET =>
        (a.keyId, a.secretKey) match {
          case (Some(k), Some(s)) =>
            Static(k, s, a.sessionToken, SOURCE_SECRET, requestToken(a.sessionToken, hasRole, a.stsDisabled))
          case _ => 
            throw new IllegalArgumentException(s"invalid auth uri: ${a}: ACCESS_KEY_ID and SECRET_ACCESS_KEY are required: secret://<ACCESS_KEY_ID>:<SECRET_ACCESS_KEY>[:SESSION_TOKEN]")
        }

      case AuthURI.ENV =>
        envStatic(uri, a, hasRole)

      case AuthURI.PROFILE =>
        Profile(a.profile.orElse(a.env.get("AWS_PROFILE").filter(_.nonEmpty)).getOrElse("default"), a.file)

      case AuthURI.ROLE =>
        // role of the environment (task, pod, instance) 
        Default("role")
    }

    a.roleArn match {
      case Some(arn) => Role(arn, a.externalId, a.session.getOrElse(DEF_ROLE_SESSION), duration(a.duration), base)
      case None => base
    }
  }

  private def envStatic(uri: CloudURI, a: AuthURI, hasRole: Boolean): Static = {
    val (kn, sn, tn) = a.envNames
    // default names of the Cloud (AWS_ACCESS_KEY, AWS_ACCESS_SECRET, AWS_SESSION_TOKEN, ...): key and secret are always a pair
    val defaults = CloudURI(s"${uri.cloud}://", a.env)

    def named(n: String): String = 
      a.env.get(n).filter(_.nonEmpty).getOrElse(throw new IllegalArgumentException(s"invalid auth uri: ${a}: environment variable '${n}' is not defined"))

    val key = kn.map(named).orElse(defaults.accessKey)
    val secret = sn.map(named).orElse(defaults.accessSecret)

    (key, secret) match {
      case (Some(k), Some(s)) =>
        // token of the default variable belongs to the default key: it is never mixed with custom key or secret
        val token = tn.flatMap(n => a.env.get(n).filter(_.nonEmpty)).orElse(
          if (kn.isEmpty && sn.isEmpty && tn.isEmpty) defaults.sessionToken else None
        )
        val names = Seq(kn, sn, tn).flatten.mkString(",")
        Static(k, s, token, SOURCE_ENV, requestToken(token, hasRole, a.stsDisabled),
          if (names.nonEmpty) names else defaults.envCredentialsNames.getOrElse(""))

      case _ =>
        throw new IllegalArgumentException(s"invalid auth uri: ${a}: ${uri.cloud.toUpperCase}_ACCESS_KEY and ${uri.cloud.toUpperCase}_ACCESS_SECRET environment variables are not defined")
    }
  }
}
