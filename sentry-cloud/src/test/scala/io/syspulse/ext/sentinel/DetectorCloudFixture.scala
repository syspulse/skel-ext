package io.syspulse.ext.sentinel

import spray.json._

import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Config
import io.syspulse.skel.plugin.PluginDescriptor

/** Creates DetectorCloud (or a specialized Detector) from DetectorConfig (as Sentinel does) */
trait DetectorCloudFixture {

  def createRun(confJson: String, detector: DetectorCloud = new DetectorCloud(PluginDescriptor("DetectorCloud", "1.0.0", "DetectorCloud"))): (DetectorCloud, DetectorConfig, SentryRun0) = {
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
      name = detector.did,
      source = "test",
      tags = Seq.empty,
      config = Some(confJson.parseJson.asJsObject),
      destinations = Seq.empty
    )

    val rx = new SentryRun(detector, conf, new Config { override val env: String = "test" }, None)
    (detector, conf, rx)
  }

  /** onInit + onStart. Returns status of onStart */
  def start(confJson: String, detector: DetectorCloud = new DetectorCloud(PluginDescriptor("DetectorCloud", "1.0.0", "DetectorCloud"))) = {
    val (d, conf, rx) = createRun(confJson, detector)
    val r0 = d.onInit(rx, conf)
    assert(r0 == SentryRun.SENTRY_INIT)
    val r = d.onStart(rx, conf)
    (d, conf, rx, r)
  }
}
