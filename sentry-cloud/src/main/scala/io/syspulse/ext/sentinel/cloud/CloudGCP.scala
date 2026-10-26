package io.syspulse.ext.sentinel.cloud

import io.hacken.ext.core.Severity

/**
 * GCP Cloud (MOCKED). 
 * 
 * Real implementation is planned: Cloud Monitoring (alert policies), Cloud IAM (audit logs), Cloud Storage
 * Credentials: gcp://ACCESS_KEY:ACCESS_SECRET@host?project=...  or GCP_ACCESS_KEY, GCP_ACCESS_SECRET
 */
class CloudGCP(uri0: String) extends CloudMocked(uri0) {

  def provider: String = "gcp"

  protected def sample(system: String): Seq[CloudAlert] = {
    val project = uri.param("project", "mock-project")
    val ts = System.currentTimeMillis()
    system match {
      case Cloud.SYSTEM_WATCH =>
        Seq(CloudAlert(system, s"gcp:${project}:policy:cpu-high", "cpu-high", "FIRING", Severity.HIGH,
          "Cloud Monitoring policy 'cpu-high' is firing", ts,
          Map("provider" -> provider, "project" -> project, "policy" -> "cpu-high", "mock" -> "true")))
      case Cloud.SYSTEM_IAM =>
        Seq(CloudAlert(system, s"gcp:${project}:iam:owner-binding", "serviceAccount:mock@mock-project.iam.gserviceaccount.com", "ROLE_OWNER", Severity.CRITICAL,
          "Service account has roles/owner", ts,
          Map("provider" -> provider, "project" -> project, "role" -> "roles/owner", "mock" -> "true")))
      case Cloud.SYSTEM_TRAIL =>
        Seq(CloudAlert(system, s"gcp:${project}:audit:SetIamPolicy", "my-project", "SetIamPolicy", Severity.HIGH,
          "Audit log: SetIamPolicy", ts,
          Map("provider" -> provider, "project" -> project, "event" -> "SetIamPolicy", "mock" -> "true")))
      case _ =>
        Seq(CloudAlert(system, s"gcp:${project}:bucket:mock-bucket", "mock-bucket", "UNAVAILABLE", Severity.CRITICAL,
          "Bucket 'mock-bucket' is unavailable", ts,
          Map("provider" -> provider, "project" -> project, "bucket" -> "mock-bucket", "mock" -> "true")))
    }
  }
}
