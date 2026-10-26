package io.syspulse.ext.sentinel.cloud

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CloudURISpec extends AnyFlatSpec with Matchers {

  val envEmpty = Map.empty[String, String]

  "CloudURI" should "parse empty aws:// URI" in {
    val u = CloudURI("aws://", envEmpty)
    u.cloud shouldBe "aws"
    u.host shouldBe None
    u.params shouldBe empty
    u.accessKey shouldBe None
    u.accessSecret shouldBe None
    u.sessionToken shouldBe None
    u.region shouldBe None
    u.hasCredentials shouldBe false
    u.endpoint shouldBe None
  }

  it should "parse params" in {
    val u = CloudURI("aws://?region=eu-west-1&states=ALARM,INSUFFICIENT_DATA&flag", envEmpty)
    u.region shouldBe Some("eu-west-1")
    u.param("states") shouldBe "ALARM,INSUFFICIENT_DATA"
    u.paramList("states") shouldBe Seq("ALARM", "INSUFFICIENT_DATA")
    u.param("flag", "x") shouldBe ""
    u.param("missing", "def") shouldBe "def"
    u.paramList("missing") shouldBe empty
  }

  it should "parse key and secret" in {
    val u = CloudURI("aws://AKIAKEY:SECRET@?region=us-east-1", envEmpty)
    u.accessKey shouldBe Some("AKIAKEY")
    u.accessSecret shouldBe Some("SECRET")
    u.sessionToken shouldBe None
    u.host shouldBe None
    u.hasCredentials shouldBe true
    u.region shouldBe Some("us-east-1")
  }

  it should "parse key, secret and session token" in {
    val u = CloudURI("aws://AKIAKEY:SECRET:TOKEN@monitoring.eu-west-1.amazonaws.com?region=eu-west-1", envEmpty)
    u.accessKey shouldBe Some("AKIAKEY")
    u.accessSecret shouldBe Some("SECRET")
    u.sessionToken shouldBe Some("TOKEN")
    u.host shouldBe Some("monitoring.eu-west-1.amazonaws.com")
    u.endpoint shouldBe Some("https://monitoring.eu-west-1.amazonaws.com")
  }

  it should "keep special characters in secret" in {
    val u = CloudURI("aws://AKIAKEY:ab/cd+ef=gh@?region=us-east-1", envEmpty)
    u.accessSecret shouldBe Some("ab/cd+ef=gh")
  }

  it should "decode url-encoded credentials" in {
    val u = CloudURI("aws://AKIAKEY:ab%2Fcd@?region=us-east-1", envEmpty)
    u.accessSecret shouldBe Some("ab/cd")
  }

  it should "parse custom host without credentials" in {
    val u = CloudURI("aws://localhost:4566?region=us-east-1&ssl=false", envEmpty)
    u.accessKey shouldBe None
    u.host shouldBe Some("localhost:4566")
    u.endpoint shouldBe Some("http://localhost:4566")
  }

  it should "use environment variables with cloud prefix" in {
    val env = Map(
      "AWS_ACCESS_KEY" -> "ENVKEY", "AWS_ACCESS_SECRET" -> "ENVSECRET", "AWS_SESSION_TOKEN" -> "ENVTOKEN", "AWS_REGION" -> "eu-central-1",
      "GCP_ACCESS_KEY" -> "GCPKEY", "AZURE_ACCESS_KEY" -> "AZKEY"
    )

    val aws = CloudURI("aws://", env)
    aws.prefix shouldBe "AWS_"
    aws.accessKey shouldBe Some("ENVKEY")
    aws.accessSecret shouldBe Some("ENVSECRET")
    aws.sessionToken shouldBe Some("ENVTOKEN")
    aws.region shouldBe Some("eu-central-1")

    CloudURI("gcp://", env).accessKey shouldBe Some("GCPKEY")
    CloudURI("gcp://", env).prefix shouldBe "GCP_"
    CloudURI("azure://", env).accessKey shouldBe Some("AZKEY")
    CloudURI("azure://", env).prefix shouldBe "AZURE_"
  }

  it should "prefer URI over environment" in {
    val env = Map("AWS_ACCESS_KEY" -> "ENVKEY", "AWS_ACCESS_SECRET" -> "ENVSECRET", "AWS_REGION" -> "eu-central-1")
    val u = CloudURI("aws://URIKEY:URISECRET@?region=us-west-2", env)
    u.accessKey shouldBe Some("URIKEY")
    u.accessSecret shouldBe Some("URISECRET")
    u.region shouldBe Some("us-west-2")
  }

  it should "mix URI key and environment secret" in {
    val env = Map("AWS_ACCESS_SECRET" -> "ENVSECRET")
    val u = CloudURI("aws://URIKEY@", env)
    u.accessKey shouldBe Some("URIKEY")
    u.accessSecret shouldBe Some("ENVSECRET")
  }

  it should "support standard AWS environment variables" in {
    val env = Map("AWS_ACCESS_KEY_ID" -> "STDKEY", "AWS_SECRET_ACCESS_KEY" -> "STDSECRET", "AWS_DEFAULT_REGION" -> "ap-south-1")
    val u = CloudURI("aws://", env)
    u.accessKey shouldBe Some("STDKEY")
    u.accessSecret shouldBe Some("STDSECRET")
    u.region shouldBe Some("ap-south-1")
  }

  it should "expand environment variables in credentials" in {
    val env = Map("MY_KEY" -> "K1", "MY_SECRET" -> "S1")
    val u = CloudURI("aws://{MY_KEY}:{MY_SECRET}@?region=us-east-1", env)
    u.accessKey shouldBe Some("K1")
    u.accessSecret shouldBe Some("S1")
  }

  it should "handle gcp and azure" in {
    CloudURI("gcp://?project=p1", envEmpty).cloud shouldBe "gcp"
    CloudURI("gcp://?project=p1", envEmpty).param("project") shouldBe "p1"
    CloudURI("azure://?subscription=s1", envEmpty).cloud shouldBe "azure"
  }

  it should "handle invalid URI" in {
    CloudURI("not-a-uri", envEmpty).cloud shouldBe ""
    CloudURI("", envEmpty).cloud shouldBe ""
  }

  it should "never expose secrets in toString" in {
    val u = CloudURI("aws://AKIAKEY:SUPERSECRET:SESSIONTOKEN@?region=us-east-1", envEmpty)
    u.toString should not include "SUPERSECRET"
    u.toString should not include "SESSIONTOKEN"
    u.toString should not include "AKIAKEY"
  }
}
