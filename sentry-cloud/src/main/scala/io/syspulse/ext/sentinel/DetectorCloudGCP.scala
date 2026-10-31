package io.syspulse.ext.sentinel

import io.syspulse.skel.plugin.PluginDescriptor

/**
 * GCP Cloud Monitor. Extension of DetectorCloud with GCP schema (`schema-DetectorCloudGCP.json`):
 *   uri = gcp://, system = Cloud Monitoring | Cloud Audit Logs | Cloud IAM | Cloud Storage
 * 
 * GCP is mocked for now
 */
class DetectorCloudGCP(pd: PluginDescriptor) extends DetectorCloud(pd) {
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  override protected def cloudScheme: String = "gcp"
  override protected def defUri: String = "gcp://"
  override protected def defSystem: String = "Cloud Monitoring"
}
