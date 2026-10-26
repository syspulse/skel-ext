package io.syspulse.ext.sentinel

import spray.json._

import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Config
import io.syspulse.skel.plugin.PluginDescriptor

/** Creates DetectorCloud from DetectorConfig (as Sentinel does) */
trait DetectorCloudFixture {

  def createRun(confJson: String): (DetectorCloud, DetectorConfig, SentryRun0) = {
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
        name = "Cloud"
      ),
      schema = None,
      name = "DetectorCloud",
      source = "test",
      tags = Seq.empty,
      config = Some(confJson.parseJson.asJsObject),
      destinations = Seq.empty
    )

    val pd = PluginDescriptor("DetectorCloud", "1.0.0", "DetectorCloud")
    val detector = new DetectorCloud(pd)

    val rx = new SentryRun(detector, conf, new Config { override val env: String = "test" }, None)
    (detector, conf, rx)
  }

  /** onInit + onStart. Returns status of onStart */
  def start(confJson: String) = {
    val (detector, conf, rx) = createRun(confJson)
    val r0 = detector.onInit(rx, conf)
    assert(r0 == SentryRun.SENTRY_INIT)
    val r = detector.onStart(rx, conf)
    (detector, conf, rx, r)
  }
}
