package io.syspulse.ext.sentinel.cloud

import io.hacken.ext.core.Severity

/**
 * Azure Cloud (MOCKED). 
 * 
 * Real implementation is planned: Azure Monitor (alerts), Entra ID / Activity Log (IAM), Blob Storage
 * Credentials: azure://ACCESS_KEY:ACCESS_SECRET@host?subscription=...  or AZURE_ACCESS_KEY, AZURE_ACCESS_SECRET
 */
class CloudAzure(uri0: String) extends CloudMocked(uri0) {

  def provider: String = "azure"

  protected def sample(system: String): Seq[CloudAlert] = {
    val sub = uri.param("subscription", "mock-subscription")
    val ts = System.currentTimeMillis()
    system match {
      case Cloud.SYSTEM_WATCH =>
        Seq(CloudAlert(system, s"azure:${sub}:alert:vm-unhealthy", "vm-unhealthy", "FIRED", Severity.HIGH,
          "Azure Monitor alert 'vm-unhealthy' is fired", ts,
          Map("provider" -> provider, "subscription" -> sub, "alert" -> "vm-unhealthy", "mock" -> "true")))
      case Cloud.SYSTEM_IAM =>
        Seq(CloudAlert(system, s"azure:${sub}:iam:owner-assignment", "user:mock@mock.onmicrosoft.com", "ROLE_OWNER", Severity.CRITICAL,
          "User was assigned Owner role", ts,
          Map("provider" -> provider, "subscription" -> sub, "role" -> "Owner", "mock" -> "true")))
      case Cloud.SYSTEM_TRAIL =>
        Seq(CloudAlert(system, s"azure:${sub}:activity:roleAssignments/write", "roleAssignments", "roleAssignments/write", Severity.HIGH,
          "Activity log: roleAssignments/write", ts,
          Map("provider" -> provider, "subscription" -> sub, "event" -> "roleAssignments/write", "mock" -> "true")))
      case _ =>
        Seq(CloudAlert(system, s"azure:${sub}:container:mock-container", "mock-container", "UNAVAILABLE", Severity.CRITICAL,
          "Blob container 'mock-container' is unavailable", ts,
          Map("provider" -> provider, "subscription" -> sub, "container" -> "mock-container", "mock" -> "true")))
    }
  }
}
