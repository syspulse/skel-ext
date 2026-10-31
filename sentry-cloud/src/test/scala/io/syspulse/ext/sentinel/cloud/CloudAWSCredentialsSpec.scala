package io.syspulse.ext.sentinel.cloud

import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.PosixFilePermissions
import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.syspulse.ext.sentinel.cloud.auth.Auth

/**
 * Credentials selection and refresh. No network and no AWS account are required.
 */
class CloudAWSCredentialsSpec extends AnyFlatSpec with Matchers {

  val envEmpty = Map.empty[String, String]
  val envKeys = Map("AWS_ACCESS_KEY_ID" -> "ENVKEY", "AWS_SECRET_ACCESS_KEY" -> "ENVSECRET", "AWS_SESSION_TOKEN" -> "ENVTOKEN")
  val envKeysNoToken = envKeys - "AWS_SESSION_TOKEN"

  import CloudAWS._

  "CloudAWS credentials" should "use default chain if nothing is specified" in {
    authBase(CloudURI("aws://?region=eu-west-1", envEmpty)) shouldBe AUTH_DEFAULT
    isStatic(CloudURI("aws://", envEmpty)) shouldBe false
  }

  it should "use static keys from environment (not refreshable)" in {
    val u = CloudURI("aws://?region=eu-west-1", envKeys)
    authBase(u) shouldBe AUTH_ENV
    isStatic(u) shouldBe true
  }

  it should "use static keys from URI (highest priority)" in {
    val u = CloudURI("aws://K:S:T@?profile=dev", envKeys)
    authBase(u) shouldBe AUTH_URI
    isStatic(u) shouldBe true
    staticToken(u) shouldBe Some("T")
  }

  it should "never mix URI keys with SESSION_TOKEN from environment" in {
    val u = CloudURI("aws://K:S@?region=eu-west-1", envKeys)
    authBase(u) shouldBe AUTH_URI
    staticToken(u) shouldBe None
    isStatic(u) shouldBe false
  }

  it should "request SESSION_TOKEN if environment keys have no token" in {
    val u = CloudURI("aws://?region=eu-west-1", envKeysNoToken)
    authBase(u) shouldBe AUTH_ENV
    staticToken(u) shouldBe None
    requestsToken(u) shouldBe true
    isStatic(u) shouldBe false
    authName(u) shouldBe "env+sts"
  }

  it should "NOT request SESSION_TOKEN if it is defined" in {
    val u = CloudURI("aws://?region=eu-west-1", envKeys)
    staticToken(u) shouldBe Some("ENVTOKEN")
    requestsToken(u) shouldBe false
    isStatic(u) shouldBe true
    authName(u) shouldBe "env"
  }

  it should "request SESSION_TOKEN for keys in URI" in {
    requestsToken(CloudURI("aws://K:S@?region=eu-west-1", envEmpty)) shouldBe true
    requestsToken(CloudURI("aws://K:S:T@?region=eu-west-1", envEmpty)) shouldBe false
  }

  it should "not request SESSION_TOKEN when disabled, with role, profile or default chain" in {
    requestsToken(CloudURI("aws://?sts=false", envKeysNoToken)) shouldBe false
    requestsToken(CloudURI("aws://?role_arn=arn:aws:iam::123456789012:role/Sentry", envKeysNoToken)) shouldBe false
    requestsToken(CloudURI("aws://?profile=dev", envKeysNoToken)) shouldBe false
    requestsToken(CloudURI("aws://", envEmpty)) shouldBe false
  }

  it should "prefer profile over environment keys" in {
    val u = CloudURI("aws://?profile=dev", envKeys)
    authBase(u) shouldBe AUTH_PROFILE
    isStatic(u) shouldBe false
  }

  it should "wrap base credentials into role" in {
    authName(CloudURI("aws://?role_arn=arn:aws:iam::123456789012:role/Sentry", envEmpty)) shouldBe "role(default)"
    authName(CloudURI("aws://?profile=dev&role_arn=arn:aws:iam::123456789012:role/Sentry", envKeys)) shouldBe "role(profile)"
    authName(CloudURI("aws://?profile=dev", envKeys)) shouldBe "profile"
  }

  it should "parse role parameters" in {
    val u = CloudURI("aws://?role_arn=arn:aws:iam::123456789012:role/Sentry&external_id=abc&role_session=s1&role_duration=900", envEmpty)
    u.roleArn shouldBe Some("arn:aws:iam::123456789012:role/Sentry")
    u.param("external_id") shouldBe "abc"
    u.param("role_session") shouldBe "s1"
    u.param("role_duration") shouldBe "900"
  }

  it should "not leak secrets into auth description" in {
    val c = new CloudAWS("aws://AKIASECRETKEY:TOPSECRET:TOKEN123@?region=eu-west-1")
    c.auth shouldBe AUTH_URI
    c.auth should not include "TOPSECRET"
    c.toString should not include "TOPSECRET"
    c.toString should not include "TOKEN123"
  }

  // ----- Refresh -------------------------------------------------------------------------------------------------
  // `credential_process` is executed by AWS SDK every time cached credentials expire. 
  // That is how SSO / Vault / any external refresher is plugged in.
  private def withProfile[T](expireSec: Int)(f: (Path, Path) => T): T = {
    val dir = Files.createTempDirectory("sentry-cloud-creds")
    try {
      val counter = dir.resolve("counter")
      val script = dir.resolve("creds.sh")
      Files.writeString(script,
        s"""#!/bin/bash
           |n=$$(cat ${counter} 2>/dev/null || echo 0); n=$$((n+1)); echo $$n > ${counter}
           |exp=$$(date -u -d "+${expireSec} seconds" +%Y-%m-%dT%H:%M:%SZ)
           |echo "{\\"Version\\":1,\\"AccessKeyId\\":\\"AKIATEST$$n\\",\\"SecretAccessKey\\":\\"secret$$n\\",\\"SessionToken\\":\\"token$$n\\",\\"Expiration\\":\\"$$exp\\"}"
           |""".stripMargin)
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))

      val file = dir.resolve("credentials")
      Files.writeString(file, s"[dev]\ncredential_process = ${script}\n")
      f(file, dir)
    } finally {
      Try(Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p)))
    }
  }

  private def Try[T](f: => T) = scala.util.Try(f)

  class TestAWS(uri: String) extends CloudAWS(uri) {
    def creds = credentials
  }

  // SDK caches credentials and requests new ones shortly before expiration
  it should "cache valid credentials from profile (credential_process)" in {
    assume(Files.isExecutable(Paths.get("/bin/bash")), "bash is required")

    withProfile(expireSec = 3600) { (file, dir) =>
      val cloud = new TestAWS(s"aws://?region=eu-west-1&profile=dev&profile_file=${file}")
      try {
        cloud.auth shouldBe AUTH_PROFILE
        val c1 = cloud.creds.getCredentials
        c1.getAWSAccessKeyId should startWith("AKIATEST")
        cloud.creds.getCredentials.getAWSAccessKeyId shouldBe c1.getAWSAccessKeyId
        Files.readString(dir.resolve("counter")).trim shouldBe "1"
      } finally {
        cloud.close()
      }
    }
  }

  it should "refresh expiring credentials from profile (credential_process)" in {
    assume(Files.isExecutable(Paths.get("/bin/bash")), "bash is required")

    withProfile(expireSec = 2) { (file, dir) =>
      val cloud = new TestAWS(s"aws://?region=eu-west-1&profile=dev&profile_file=${file}")
      try {
        val c1 = cloud.creds.getCredentials.getAWSAccessKeyId
        val c2 = cloud.creds.getCredentials.getAWSAccessKeyId
        c1 should startWith("AKIATEST")
        c2 should startWith("AKIATEST")
        c2 should not be c1
      } finally {
        cloud.close()
      }
    }
  }

  class TestAuthAWS(uri: String, auth: Option[String]) extends CloudAWS(uri, auth) {
    def creds = credentials
  }

  it should "use credentials of `auth` profile:// (credential_process) instead of credentials of uri" in {
    assume(Files.isExecutable(Paths.get("/bin/bash")), "bash is required")

    withProfile(expireSec = 3600) { (file, dir) =>
      // keys of uri are ignored: auth has a preference
      val cloud = new TestAuthAWS("aws://URIKEY:URISECRET:URITOKEN@?region=eu-west-1", Some(s"profile://${file}?profile=dev"))
      try {
        cloud.auth shouldBe AUTH_PROFILE
        cloud.authSpec shouldBe Auth.Profile("dev", Some(file.toString))
        val c = cloud.creds.getCredentials
        c.getAWSAccessKeyId should startWith("AKIATEST")
        c.getAWSAccessKeyId should not be "URIKEY"
      } finally {
        cloud.close()
      }
    }
  }

  it should "use credentials of `auth` env:// and secret://" in {
    val e = new TestAuthAWS("aws://URIKEY:URISECRET:URITOKEN@?region=eu-west-1", Some("secret://AUTHKEY:AUTHSECRET:AUTHTOKEN"))
    try {
      e.auth shouldBe "secret"
      e.authSpec.isStatic shouldBe true
      val c = e.creds.getCredentials.asInstanceOf[com.amazonaws.auth.AWSSessionCredentials]
      c.getAWSAccessKeyId shouldBe "AUTHKEY"
      c.getSessionToken shouldBe "AUTHTOKEN"
    } finally e.close()
  }

  it should "fail to create Cloud with invalid `auth`" in {
    intercept[IllegalArgumentException] { new CloudAWS("aws://?region=eu-west-1", Some("ftp://x")) }
    intercept[IllegalArgumentException] { Cloud("aws://?region=eu-west-1", Some("secret://onlykey")) }
  }
}
