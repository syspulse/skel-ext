package io.syspulse.ext.sentinel

import io.syspulse.skel.plugin.PluginDescriptor

/**
 * AWS Cloud Monitor. Extension of DetectorCloud with AWS schema (`schema-DetectorCloudAWS.json`):
 *   uri = aws://, system = CloudWatch | CloudTrail | IAM | S3
 */
class DetectorCloudAWS(pd: PluginDescriptor) extends DetectorCloud(pd) {
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  override protected def cloudScheme: String = "aws"
  override protected def defUri: String = "aws://"
  override protected def defSystem: String = "CloudWatch"
}
