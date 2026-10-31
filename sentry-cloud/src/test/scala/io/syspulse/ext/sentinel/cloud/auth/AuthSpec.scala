package io.syspulse.ext.sentinel.cloud.auth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.syspulse.ext.sentinel.cloud.CloudURI

class AuthSpec extends AnyFlatSpec with Matchers {

  val envEmpty = Map.empty[String, String]
  // default names of the Cloud
  val envDefault = Map("AWS_ACCESS_KEY" -> "DEFKEY", "AWS_ACCESS_SECRET" -> "DEFSECRET", "AWS_SESSION_TOKEN" -> "DEFTOKEN")
  val envCustom = Map("MY_KEY" -> "MYKEY", "MY_SECRET" -> "MYSECRET", "MY_TOKEN" -> "MYTOKEN")

  def resolve(uri: String, auth: Option[String], env: Map[String, String] = envEmpty): Auth =
    Auth.resolve(CloudURI(uri, env), auth.map(a => AuthURI(a, env)))

  // ---- no auth: credentials from Cloud URI (compatibility)
  "Auth" should "resolve default chain from empty URI" in {
    resolve("aws://?region=eu-west-1", None) shouldBe Auth.Default()
    resolve("aws://", None).name shouldBe "default"
  }

  it should "resolve credentials from Cloud URI" in {
    val a = resolve("aws://K:S@?region=eu-west-1", None).asInstanceOf[Auth.Static]
    a.key shouldBe "K"
    a.secret shouldBe "S"
    a.token shouldBe None
    a.source shouldBe "uri"
    a.requestToken shouldBe true
    a.name shouldBe "uri+sts"
  }

  it should "resolve profile and role from Cloud URI" in {
    resolve("aws://?profile=dev&profile_file=/tmp/c", None) shouldBe Auth.Profile("dev", Some("/tmp/c"))

    val r = resolve("aws://?profile=dev&role_arn=arn:aws:iam::1:role/R&external_id=x&role_session=s&role_duration=900", None).asInstanceOf[Auth.Role]
    r.arn shouldBe "arn:aws:iam::1:role/R"
    r.externalId shouldBe Some("x")
    r.session shouldBe "s"
    r.duration shouldBe 900
    r.underlying shouldBe Auth.Profile("dev", None)
    r.name shouldBe "role(profile)"
  }

  // ---- secret://
  it should "resolve secret:// without token: token is requested" in {
    val a = resolve("aws://", Some("secret://K:S")).asInstanceOf[Auth.Static]
    a shouldBe Auth.Static("K", "S", None, "secret", true)
    a.isStatic shouldBe false
    a.name shouldBe "secret+sts"
  }

  it should "resolve secret:// with token: static" in {
    val a = resolve("aws://", Some("secret://K:S:T")).asInstanceOf[Auth.Static]
    a shouldBe Auth.Static("K", "S", Some("T"), "secret", false)
    a.isStatic shouldBe true
    a.name shouldBe "secret"
  }

  it should "disable request of the token with sts=false" in {
    resolve("aws://", Some("secret://K:S?sts=false")).asInstanceOf[Auth.Static].requestToken shouldBe false
  }

  it should "fail on incomplete secret://" in {
    val e = intercept[IllegalArgumentException] { resolve("aws://", Some("secret://K")) }
    e.getMessage should include ("required")
    e.getMessage should not include "secret://K:"
    intercept[IllegalArgumentException] { resolve("aws://", Some("secret://")) }
  }

  it should "not leak secrets in errors and names" in {
    val a = resolve("aws://", Some("secret://AKIAKEY123:TOPSECRET:TOKEN123"))
    a.toString should not include "TOPSECRET"
    a.toString should not include "TOKEN123"
  }

  // ---- env://
  it should "resolve env:// with default names" in {
    val a = resolve("aws://", Some("env://"), envDefault).asInstanceOf[Auth.Static]
    a.key shouldBe "DEFKEY"
    a.secret shouldBe "DEFSECRET"
    a.token shouldBe Some("DEFTOKEN")
    a.source shouldBe "env"
    a.isStatic shouldBe true
  }

  it should "resolve env:// with default names without token: token is requested" in {
    val a = resolve("aws://", Some("env://"), envDefault - "AWS_SESSION_TOKEN").asInstanceOf[Auth.Static]
    a.token shouldBe None
    a.requestToken shouldBe true
    a.name shouldBe "env+sts"
  }

  it should "resolve env:// with custom names" in {
    val a = resolve("aws://", Some("env://$MY_KEY:$MY_SECRET:$MY_TOKEN"), envCustom).asInstanceOf[Auth.Static]
    a.key shouldBe "MYKEY"
    a.secret shouldBe "MYSECRET"
    a.token shouldBe Some("MYTOKEN")
    a.detail shouldBe "MY_KEY,MY_SECRET,MY_TOKEN"
  }

  it should "never take token from default variable if names are custom" in {
    // default AWS_SESSION_TOKEN belongs to default keys
    val a = resolve("aws://", Some("env://$MY_KEY:$MY_SECRET"), envCustom ++ envDefault).asInstanceOf[Auth.Static]
    a.key shouldBe "MYKEY"
    a.token shouldBe None
    a.requestToken shouldBe true
  }

  it should "not fail if custom token variable is not defined: token is requested" in {
    val a = resolve("aws://", Some("env://$MY_KEY:$MY_SECRET:$MY_UNDEFINED"), envCustom).asInstanceOf[Auth.Static]
    a.token shouldBe None
    a.requestToken shouldBe true
  }

  it should "mix custom and default names" in {
    // default key and secret, custom token
    val a = resolve("aws://", Some("env://::$MY_TOKEN"), envDefault ++ envCustom).asInstanceOf[Auth.Static]
    a.key shouldBe "DEFKEY"
    a.secret shouldBe "DEFSECRET"
    a.token shouldBe Some("MYTOKEN")

    // custom key, default secret
    val b = resolve("aws://", Some("env://$MY_KEY"), envDefault ++ envCustom).asInstanceOf[Auth.Static]
    b.key shouldBe "MYKEY"
    b.secret shouldBe "DEFSECRET"
  }

  it should "use standard AWS names by default" in {
    val env = Map("AWS_ACCESS_KEY_ID" -> "STDKEY", "AWS_SECRET_ACCESS_KEY" -> "STDSECRET")
    val a = resolve("aws://", Some("env://"), env).asInstanceOf[Auth.Static]
    a.key shouldBe "STDKEY"
    a.secret shouldBe "STDSECRET"
  }

  it should "use default names of the Cloud" in {
    val env = Map("GCP_ACCESS_KEY" -> "GKEY", "GCP_ACCESS_SECRET" -> "GSECRET")
    val a = resolve("gcp://", Some("env://"), env).asInstanceOf[Auth.Static]
    a.key shouldBe "GKEY"
    a.secret shouldBe "GSECRET"
  }

  it should "fail if environment variables are not defined" in {
    val e1 = intercept[IllegalArgumentException] { resolve("aws://", Some("env://"), envEmpty) }
    e1.getMessage should include ("AWS_ACCESS_KEY")

    val e2 = intercept[IllegalArgumentException] { resolve("aws://", Some("env://$UNDEFINED:$MY_SECRET"), envCustom) }
    e2.getMessage should include ("'UNDEFINED'")
  }

  // ---- profile://
  it should "resolve profile://" in {
    resolve("aws://", Some("profile://?profile=sentry")) shouldBe Auth.Profile("sentry", None)
    resolve("aws://", Some("profile:///etc/creds?profile=sentry")) shouldBe Auth.Profile("sentry", Some("/etc/creds"))
    resolve("aws://", Some("profile://?profile=sentry")).isStatic shouldBe false
  }

  it should "use AWS_PROFILE or `default` for profile://" in {
    resolve("aws://", Some("profile://"), Map("AWS_PROFILE" -> "from-env")) shouldBe Auth.Profile("from-env", None)
    resolve("aws://", Some("profile://")) shouldBe Auth.Profile("default", None)
    // parameter has priority
    resolve("aws://", Some("profile://?profile=p1"), Map("AWS_PROFILE" -> "from-env")) shouldBe Auth.Profile("p1", None)
  }

  // ---- role://
  it should "resolve role:// as IAM role of the environment" in {
    val a = resolve("aws://", Some("role://"))
    a shouldBe Auth.Default("role")
    a.name shouldBe "role"
    a.isStatic shouldBe false
  }

  it should "resolve role:// with role to assume" in {
    val a = resolve("aws://", Some("role://?arn=arn:aws:iam::123456789012:role/Sentry&external_id=abc&session=s1&duration=1800")).asInstanceOf[Auth.Role]
    a.arn shouldBe "arn:aws:iam::123456789012:role/Sentry"
    a.externalId shouldBe Some("abc")
    a.session shouldBe "s1"
    a.duration shouldBe 1800
    a.underlying shouldBe Auth.Default("role")
    a.name shouldBe "role(role)"
  }

  it should "use default session and duration of the role" in {
    val a = resolve("aws://", Some("role://?arn=arn:aws:iam::1:role/R")).asInstanceOf[Auth.Role]
    a.session shouldBe Auth.DEF_ROLE_SESSION
    a.duration shouldBe Auth.DEF_ROLE_DURATION
    a.externalId shouldBe None
    // invalid duration
    resolve("aws://", Some("role://?arn=arn:aws:iam::1:role/R&duration=abc")).asInstanceOf[Auth.Role].duration shouldBe Auth.DEF_ROLE_DURATION
  }

  it should "assume the role with credentials of any scheme" in {
    val a = resolve("aws://", Some("secret://K:S?arn=arn:aws:iam::1:role/R")).asInstanceOf[Auth.Role]
    a.underlying shouldBe Auth.Static("K", "S", None, "secret", false)   // keys are used to assume the role: token is not requested
    a.name shouldBe "role(secret)"

    resolve("aws://", Some("profile://?profile=p&arn=arn:aws:iam::1:role/R")).name shouldBe "role(profile)"
    resolve("aws://", Some("env://?arn=arn:aws:iam::1:role/R"), envDefault).name shouldBe "role(env)"
  }

  // ---- priority
  it should "prefer `auth` over credentials in Cloud URI" in {
    val uri = "aws://URIKEY:URISECRET:URITOKEN@?region=eu-west-1&profile=dev&role_arn=arn:aws:iam::1:role/FromUri&sts=false"

    // everything from URI
    resolve(uri, None).name shouldBe "role(uri)"

    // auth replaces keys, profile and role of URI
    val a = resolve(uri, Some("secret://AUTHKEY:AUTHSECRET"), envDefault).asInstanceOf[Auth.Static]
    a.key shouldBe "AUTHKEY"
    a.source shouldBe "secret"
    a.requestToken shouldBe true

    resolve(uri, Some("profile://?profile=sentry")) shouldBe Auth.Profile("sentry", None)
    resolve(uri, Some("role://")) shouldBe Auth.Default("role")
    resolve(uri, Some("env://"), envDefault).base.asInstanceOf[Auth.Static].key shouldBe "DEFKEY"
  }

  it should "prefer `auth` over environment keys" in {
    resolve("aws://", None, envDefault).base.asInstanceOf[Auth.Static].source shouldBe "env"
    resolve("aws://", Some("role://"), envDefault) shouldBe Auth.Default("role")
    resolve("aws://", Some("profile://?profile=p"), envDefault) shouldBe Auth.Profile("p", None)
  }

  it should "use Cloud URI if auth is not specified" in {
    resolve("aws://K:S@", None).name shouldBe "uri+sts"
  }

  // ---- invalid
  it should "fail on unsupported auth" in {
    val e = intercept[IllegalArgumentException] { resolve("aws://", Some("ftp://host")) }
    e.getMessage should include ("ftp://")
    e.getMessage should include ("secret://")

    val e2 = intercept[IllegalArgumentException] { resolve("aws://", Some("not-a-uri")) }
    e2.getMessage should include ("invalid auth uri")
  }
}
