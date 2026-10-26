package io.syspulse.ext.sentinel.cloud

import scala.concurrent.Await
import scala.concurrent.duration._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CloudMockSpec extends AnyFlatSpec with Matchers {

  def await[T](f: scala.concurrent.Future[T]): T = Await.result(f, 5.seconds)

  "CloudGCP" should "return mocked findings for every system" in {
    val c = new CloudGCP("gcp://?project=p1")
    c.provider shouldBe "gcp"
    Cloud.SYSTEMS.foreach { s =>
      val aa = await(c.check(s))
      aa should have size 1
      aa.head.system shouldBe s
      aa.head.meta("mock") shouldBe "true"
      aa.head.meta("provider") shouldBe "gcp"
    }
  }

  "CloudAzure" should "return mocked findings for every system" in {
    val c = new CloudAzure("azure://?subscription=s1")
    c.provider shouldBe "azure"
    Cloud.SYSTEMS.foreach { s =>
      val aa = await(c.check(s))
      aa should have size 1
      aa.head.system shouldBe s
      aa.head.meta("provider") shouldBe "azure"
    }
  }

  "Mocked cloud" should "return nothing for mock=ok" in {
    await(new CloudGCP("gcp://?mock=ok").check("CloudWatch")) shouldBe empty
    await(new CloudAzure("azure://?mock=ok").check("IAM")) shouldBe empty
  }

  it should "fail for mock=fail" in {
    an[Exception] should be thrownBy await(new CloudGCP("gcp://?mock=fail").check("Storage"))
  }

  it should "fail for unknown system" in {
    an[IllegalArgumentException] should be thrownBy await(new CloudGCP("gcp://").check("Unknown"))
  }

  it should "have stable ids between checks" in {
    val c = new CloudGCP("gcp://?project=p1")
    await(c.check("IAM")).map(_.id) shouldBe await(c.check("IAM")).map(_.id)
  }
}
