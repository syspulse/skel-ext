package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json._
import scala.io.Source

import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Config
import io.syspulse.skel.plugin.PluginDescriptor
import io.syspulse.ext.sentinel.feeds.NewsPost

class DetectorWarSpec extends AnyFlatSpec with Matchers {

  private def getResourcePath(resource: String): String = {
    getClass.getResource(resource).getPath
  }

  private def readResourceAsString(resource: String): String = {
    val source = Source.fromResource(resource.stripPrefix("/"))
    try source.mkString finally source.close()
  }

  "DetectorWar.flattenJson" should "flatten simple JSON with string values" in {
    val json = """{"name": "John", "age": "30"}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("name" -> "John")
    result should contain ("age" -> "30")
  }

  it should "flatten simple JSON with number values" in {
    val json = """{"count": 123, "value": 45.67}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("count" -> "123")
    result should contain ("value" -> "45.67")
  }

  it should "flatten nested JSON objects" in {
    val json = """{"user": {"name": "John", "age": 30}}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("user.name" -> "John")
    result should contain ("user.age" -> "30")
  }

  it should "flatten deeply nested JSON objects" in {
    val json = """{"losses": {"personnel": {"count": 120569, "change": 1180}}}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("losses.personnel.count" -> "120569")
    result should contain ("losses.personnel.change" -> "1180")
  }

  it should "flatten JSON arrays as compact strings" in {
    val json = """{"items": [1, 2, 3]}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("items" -> "[1,2,3]")
  }

  it should "flatten mixed nested structures" in {
    val json = """{"data": {"values": [1, 2], "name": "test"}}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("data.values" -> "[1,2]")
    result should contain ("data.name" -> "test")
  }

  it should "handle empty objects" in {
    val json = """{}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result shouldBe empty
  }

  it should "flatten GSUA-1.json structure correctly" in {
    val jsonString = readResourceAsString("GSUA-1.json")
    val json = jsonString.parseJson
    val result = DetectorWar.flattenJson(json)

    // Check top-level fields
    result should contain ("date_range" -> "24.02.22 — 29.12.2025")
    result should contain ("source" -> "GSUAs")

    // Check nested losses fields
    result should contain ("losses.personnel.count" -> "120569")
    result should contain ("losses.personnel.change" -> "1180")
    result should contain ("losses.aircraft.count" -> "434")
    result should contain ("losses.helicopters.count" -> "347")
    result should contain ("losses.operational_tactical_UAVs.count" -> "96532")
    result should contain ("losses.operational_tactical_UAVs.change" -> "305")
    result should contain ("losses.cruise_missiles.count" -> "4136")
    result should contain ("losses.armored_combat_vehicles.count" -> "23877")
    result should contain ("losses.armored_combat_vehicles.change" -> "6")
    result should contain ("losses.artillery_systems.count" -> "35570")
    result should contain ("losses.artillery_systems.change" -> "13")
    result should contain ("losses.MLRS.count" -> "1581")
    result should contain ("losses.air_defense_systems.count" -> "1264")
    result should contain ("losses.ships_boats.count" -> "28")
    result should contain ("losses.submarines.count" -> "2")
    result should contain ("losses.automotive_and_fuel_tanks.count" -> "71891")
    result should contain ("losses.automotive_and_fuel_tanks.change" -> "113")
    result should contain ("losses.special_equipment.count" -> "4030")
    result should contain ("losses.special_equipment.change" -> "1")
  }

  it should "handle JSON with missing fields gracefully" in {
    val json = """{"losses": {"personnel": {"count": 120569}}}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("losses.personnel.count" -> "120569")
    result should not contain key("losses.personnel.change")
  }

  it should "handle partial data structures" in {
    val json = """{"date_range": "test", "losses": {"aircraft": {"count": 100}}}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("date_range" -> "test")
    result should contain ("losses.aircraft.count" -> "100")
    result.size shouldBe 2
  }

  it should "handle nested objects with only some fields present" in {
    val json = """{
      "losses": {
        "personnel": {"count": 1000},
        "aircraft": {"count": 50, "change": 2}
      }
    }""".parseJson
    val result = DetectorWar.flattenJson(json)

    result should contain ("losses.personnel.count" -> "1000")
    result should not contain key("losses.personnel.change")
    result should contain ("losses.aircraft.count" -> "50")
    result should contain ("losses.aircraft.change" -> "2")
  }

  it should "strip quotes from string values" in {
    val json = """{"text": "hello world"}""".parseJson
    val result = DetectorWar.flattenJson(json)

    // The value should not have surrounding quotes
    result("text") should not startWith "\""
    result("text") should not endWith "\""
    result should contain ("text" -> "hello world")
  }

  it should "preserve numbers without quotes" in {
    val json = """{"count": 123}""".parseJson
    val result = DetectorWar.flattenJson(json)

    result("count") should not contain "\""
    result should contain ("count" -> "123")
  }

  "DetectorWar" should "be instantiable with PluginDescriptor" in {
    val pd = PluginDescriptor("DetectorWar", "1.0.0", "War Report Detector")
    val detector = new DetectorWar(pd)

    detector.did shouldBe "DetectorWar"
    detector.toString should include("DetectorWar")
  }

  it should "have hardcoded configuration constants" in {
    DetectorWar.TWITTER_ACCOUNT shouldBe "twitter://GeneralStaffUA"
    DetectorWar.DEF_MAX_POSTS shouldBe 15
    DetectorWar.DEF_MAX_SEEN_POSTS shouldBe 100
    DetectorWar.DEF_DESC shouldBe "Losses Report: {loss_personnel_total}, {loss_aircraft_total}, {loss_UAV_total}, {loss_APC_total}, {loss_MLRS_total}, {loss_SAM_total}, {loss_ship_total}, {loss_submarine_total}, {loss_automotive_fuel_truck_total}, {loss_tank_total}, {loss_special_equipment_total}, {loss_cruise_missile_total}, {loss_artillery_total}, {loss_personnel_total}"
  }

  "DetectorWar.createPostAlert" should "extract AI response from 'result' field with full GSUA-1.json content" in {
    val pd = PluginDescriptor("DetectorWar", "1.0.0", "War Report Detector")
    val detector = new DetectorWar(pd)

    // Load the actual GSUA-1.json file from test resources
    val aiResponseJson = readResourceAsString("GSUA-1.json")

    val post = NewsPost(
      id = "test-post-1",
      title = "War Report Update",
      link = "https://example.com/post/1",
      author = "General Staff",
      publishedDate = System.currentTimeMillis(),
      summary = "Daily war report",
      source = "twitter://GeneralStaffUA",
      typ = "twitter",
      categories = List.empty,
      images = List.empty,
      feedMetadata = Map.empty,
      result = Map("result" -> aiResponseJson)  // Full GSUA-1.json content in "result" field
    )

    // Create a minimal SentryRun for testing
    val conf = DetectorConfig(
      id = 1,
      createdAt = System.currentTimeMillis(),
      updatedAt = System.currentTimeMillis(),
      status = "active",
      contract = io.hacken.ext.detector.DetectorConfigContract(
        id = 1,
        createdAt = System.currentTimeMillis(),
        updatedAt = System.currentTimeMillis(),
        projectId = 1,
        tenantId = 1,
        chainUid = None,
        proxyAddress = None,
        implementation = None,
        address = None,
        name = "test-contract"
      ),
      schema = None,
      name = "test-detector",
      source = "test",
      tags = Seq.empty,
      config = None,
      destinations = Seq.empty
    )

    val config = new Config {
      override val env: String = "test"
    }

    val rx = new SentryRun(detector, conf, config, None)
    rx.set("desc", DetectorWar.DEF_DESC)

    // Call createPostAlert
    val event = detector.createPostAlert(rx, post, latency = 1000L)

    // Verify event was created
    event should not be null
    event.did shouldBe "DetectorWar"

    // Verify basic metadata
    event.metadata should contain ("type" -> "twitter")
    event.metadata should contain ("id" -> "test-post-1")
    event.metadata should contain ("title" -> "War Report Update")
    event.metadata should contain ("author" -> "General Staff")
    event.metadata should contain ("latency" -> "1000")

    // Verify ALL flattened AI response fields from GSUA-1.json are present
    event.metadata should contain ("date_range" -> "24.02.22 — 29.12.2025")
    event.metadata should contain ("source" -> "GSUAs")

    // Personnel
    event.metadata should contain ("losses.personnel.count" -> "120569")
    event.metadata should contain ("losses.personnel.change" -> "1180")

    // Aircraft
    event.metadata should contain ("losses.aircraft.count" -> "434")

    // Helicopters
    event.metadata should contain ("losses.helicopters.count" -> "347")

    // UAVs
    event.metadata should contain ("losses.operational_tactical_UAVs.count" -> "96532")
    event.metadata should contain ("losses.operational_tactical_UAVs.change" -> "305")

    // Cruise missiles
    event.metadata should contain ("losses.cruise_missiles.count" -> "4136")

    // Armored vehicles
    event.metadata should contain ("losses.armored_combat_vehicles.count" -> "23877")
    event.metadata should contain ("losses.armored_combat_vehicles.change" -> "6")

    // Artillery
    event.metadata should contain ("losses.artillery_systems.count" -> "35570")
    event.metadata should contain ("losses.artillery_systems.change" -> "13")

    // MLRS
    event.metadata should contain ("losses.MLRS.count" -> "1581")

    // Air defense
    event.metadata should contain ("losses.air_defense_systems.count" -> "1264")

    // Ships/boats
    event.metadata should contain ("losses.ships_boats.count" -> "28")

    // Submarines
    event.metadata should contain ("losses.submarines.count" -> "2")

    // Automotive
    event.metadata should contain ("losses.automotive_and_fuel_tanks.count" -> "71891")
    event.metadata should contain ("losses.automotive_and_fuel_tanks.change" -> "113")

    // Special equipment
    event.metadata should contain ("losses.special_equipment.count" -> "4030")
    event.metadata should contain ("losses.special_equipment.change" -> "1")

    // Verify we have the flattened fields, not the raw AI response
    event.metadata should not contain key("ai_raw")
  }

  it should "store raw AI response in 'ai_raw' when JSON parsing fails" in {
    val pd = PluginDescriptor("DetectorWar", "1.0.0", "War Report Detector")
    val detector = new DetectorWar(pd)

    val post = NewsPost(
      id = "test-post-3",
      title = "Post with invalid JSON",
      link = "https://example.com/post/3",
      author = "Author",
      publishedDate = System.currentTimeMillis(),
      summary = "Summary",
      source = "twitter://test",
      typ = "twitter",
      categories = List.empty,
      images = List.empty,
      feedMetadata = Map.empty,
      result = Map("result" -> "invalid json {[}]")  // Invalid JSON
    )

    val conf = DetectorConfig(
      id = 1,
      createdAt = System.currentTimeMillis(),
      updatedAt = System.currentTimeMillis(),
      status = "active",
      contract = io.hacken.ext.detector.DetectorConfigContract(
        id = 1,
        createdAt = System.currentTimeMillis(),
        updatedAt = System.currentTimeMillis(),
        projectId = 1,
        tenantId = 1,
        chainUid = None,
        proxyAddress = None,
        implementation = None,
        address = None,
        name = "test-contract"
      ),
      schema = None,
      name = "test-detector",
      source = "test",
      tags = Seq.empty,
      config = None,
      destinations = Seq.empty
    )

    val config = new Config {
      override val env: String = "test"
    }

    val rx = new SentryRun(detector, conf, config, None)
    rx.set("desc", DetectorWar.DEF_DESC)

    // Should not throw exception
    val event = detector.createPostAlert(rx, post, latency = 500L)

    event should not be null
    event.metadata should contain ("id" -> "test-post-3")
    event.metadata should contain ("ai_raw" -> "invalid json {[}]")
    event.metadata should not contain key("date_range")
  }
}
