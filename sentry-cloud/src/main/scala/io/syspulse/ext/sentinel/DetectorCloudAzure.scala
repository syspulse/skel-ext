package io.syspulse.ext.sentinel

import io.syspulse.skel.plugin.PluginDescriptor

/**
 * Azure Cloud Monitor. Extension of DetectorCloud with Azure schema (`schema-DetectorCloudAzure.json`):
 *   uri = azure://, system = Azure Monitor | Activity Log | Entra ID | Blob Storage
 * 
 * Azure is mocked for now
 */
class DetectorCloudAzure(pd: PluginDescriptor) extends DetectorCloud(pd) {
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  override protected def cloudScheme: String = "azure"
  override protected def defUri: String = "azure://"
  override protected def defSystem: String = "Azure Monitor"
}
