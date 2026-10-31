package io.syspulse.ext.sentinel.cloud.auth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AuthURISpec extends AnyFlatSpec with Matchers {

  val env = Map("MY_KEY" -> "ENVKEY", "MY_SECRET" -> "ENV/SECRET+1=")

  "AuthURI" should "parse secret:// with key and secret" in {
    val a = AuthURI("secret://AKIAKEY:SECRET", env)
    a.scheme shouldBe "secret"
    a.isSupported shouldBe true
    a.keyId shouldBe Some("AKIAKEY")
    a.secretKey shouldBe Some("SECRET")
    a.sessionToken shouldBe None
    a.params shouldBe empty
  }

  it should "parse secret:// with session token" in {
    val a = AuthURI("secret://AKIAKEY:SECRET:TOKEN", env)
    a.keyId shouldBe Some("AKIAKEY")
    a.secretKey shouldBe Some("SECRET")
    a.sessionToken shouldBe Some("TOKEN")
  }

  it should "parse secret:// with empty optional session token" in {
    val a = AuthURI("secret://AKIAKEY:SECRET:", env)
    a.secretKey shouldBe Some("SECRET")
    a.sessionToken shouldBe None
  }

  it should "parse secret:// with special characters of the secret" in {
    val a = AuthURI("secret://AKIAKEY:abc/DEF+ghi=:tok/en+==", env)
    a.secretKey shouldBe Some("abc/DEF+ghi=")
    a.sessionToken shouldBe Some("tok/en+==")

    // url-encoded
    AuthURI("secret://K:a%2Fb", env).secretKey shouldBe Some("a/b")
  }

  it should "expand environment variables in secret://" in {
    val a = AuthURI("secret://{MY_KEY}:${MY_SECRET}", env)
    a.keyId shouldBe Some("ENVKEY")
    a.secretKey shouldBe Some("ENV/SECRET+1=")
  }

  it should "parse incomplete secret://" in {
    val a = AuthURI("secret://AKIAKEY", env)
    a.keyId shouldBe Some("AKIAKEY")
    a.secretKey shouldBe None
    AuthURI("secret://", env).keyId shouldBe None
  }

  it should "parse parameters" in {
    val a = AuthURI("secret://K:S?arn=arn:aws:iam::123456789012:role/Sentry&external_id=abc&sts=false&flag", env)
    a.keyId shouldBe Some("K")
    a.secretKey shouldBe Some("S")
    a.params should have size 4
    a.roleArn shouldBe Some("arn:aws:iam::123456789012:role/Sentry")
    a.externalId shouldBe Some("abc")
    a.stsDisabled shouldBe true
    a.param("flag", "x") shouldBe ""
    a.param("missing", "def") shouldBe "def"
  }

  it should "support aliases of role parameters" in {
    val a = AuthURI("role://?role_arn=arn:aws:iam::1:role/R&role_session=s1&role_duration=900", env)
    a.roleArn shouldBe Some("arn:aws:iam::1:role/R")
    a.session shouldBe Some("s1")
    a.duration shouldBe Some("900")

    val b = AuthURI("role://?arn=arn:aws:iam::1:role/R&session=s2&duration=1800", env)
    b.session shouldBe Some("s2")
    b.duration shouldBe Some("1800")
  }

  it should "parse env:// with default names" in {
    val a = AuthURI("env://", env)
    a.scheme shouldBe "env"
    a.envNames shouldBe (None, None, None)
  }

  it should "parse env:// with names" in {
    AuthURI("env://$MY_KEY:$MY_SECRET", env).envNames shouldBe (Some("MY_KEY"), Some("MY_SECRET"), None)
    AuthURI("env://$MY_KEY:$MY_SECRET:$MY_TOKEN", env).envNames shouldBe (Some("MY_KEY"), Some("MY_SECRET"), Some("MY_TOKEN"))
    // `$` is optional, `${NAME}` is supported
    AuthURI("env://MY_KEY:${MY_SECRET}:MY_TOKEN", env).envNames shouldBe (Some("MY_KEY"), Some("MY_SECRET"), Some("MY_TOKEN"))
  }

  it should "parse env:// with omitted names" in {
    AuthURI("env://::$MY_TOKEN", env).envNames shouldBe (None, None, Some("MY_TOKEN"))
    AuthURI("env://$MY_KEY::", env).envNames shouldBe (Some("MY_KEY"), None, None)
    AuthURI("env://:$MY_SECRET", env).envNames shouldBe (None, Some("MY_SECRET"), None)
  }

  it should "parse env:// with parameters" in {
    val a = AuthURI("env://$MY_KEY:$MY_SECRET?sts=false&arn=arn:aws:iam::1:role/R", env)
    a.envNames shouldBe (Some("MY_KEY"), Some("MY_SECRET"), None)
    a.stsDisabled shouldBe true
    a.roleArn shouldBe Some("arn:aws:iam::1:role/R")
  }

  it should "parse profile:// with default file" in {
    val a = AuthURI("profile://", env)
    a.scheme shouldBe "profile"
    a.file shouldBe None
    a.profile shouldBe None
  }

  it should "parse profile:// with file and profile" in {
    val a = AuthURI("profile:///etc/sentry/credentials?profile=sentry", env)
    a.file shouldBe Some("/etc/sentry/credentials")
    a.profile shouldBe Some("sentry")

    AuthURI("profile://?profile=dev", env).file shouldBe None
    AuthURI("profile://./creds", env).file shouldBe Some("./creds")
  }

  it should "expand home directory in profile://" in {
    val home = sys.props("user.home")
    AuthURI("profile://~/.aws/credentials?profile=dev", env).file shouldBe Some(s"${home}/.aws/credentials")
  }

  it should "parse role://" in {
    val a = AuthURI("role://", env)
    a.scheme shouldBe "role"
    a.roleArn shouldBe None
    a.params shouldBe empty

    val b = AuthURI("role://?arn=arn:aws:iam::123456789012:role/Sentry&external_id=abc", env)
    b.roleArn shouldBe Some("arn:aws:iam::123456789012:role/Sentry")
    b.externalId shouldBe Some("abc")
  }

  it should "ignore case of the scheme" in {
    AuthURI("SECRET://K:S", env).scheme shouldBe "secret"
    AuthURI("Role://", env).scheme shouldBe "role"
  }

  it should "detect invalid and unsupported URI" in {
    AuthURI("not-a-uri", env).scheme shouldBe ""
    AuthURI("not-a-uri", env).isSupported shouldBe false
    AuthURI("", env).isSupported shouldBe false
    AuthURI("ftp://host", env).isSupported shouldBe false
    AuthURI("ftp://host", env).scheme shouldBe "ftp"
  }

  it should "never leak secrets" in {
    val s = AuthURI("secret://AKIAKEY123:TOPSECRET:TOKEN123?arn=arn:aws:iam::1:role/R", env).toString
    s should not include "TOPSECRET"
    s should not include "TOKEN123"
    s should not include "AKIAKEY123"
    s should include ("AKIA***")
    s should include ("arn=arn:aws:iam::1:role/R")

    AuthURI("env://$MY_KEY:$MY_SECRET", env).toString shouldBe "AuthURI(env://$MY_KEY:$MY_SECRET)"
  }
}
