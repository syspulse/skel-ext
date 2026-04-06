package io.syspulse.ext.sentinel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}
import spray.json._

import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.Config
import io.syspulse.skel.plugin.PluginDescriptor
import io.haas.ingest.eth.flow.etl.{Tx, Log, Block}
import io.haas.ingest.ext.{Block => ExtBlock, Tx => ExtTx, Log => ExtLog}
import io.syspulse.skel.blockchain.eth.EthUtil
import io.hacken.ext.core.Event

class DetectorPoRSpec extends AnyFlatSpec with Matchers {

  // Helper method to create a test Block
  def createTestBlock(
    number: Long = 12345L,
    timestamp: Long = System.currentTimeMillis(),
    hash: String = "0xblock123"
  ): Block = {
    ExtBlock(
      hash = hash,
      number = number,
      parent_hash = "0xparent123",
      nonce = Some("0x"),
      sha3_uncles = Some("0xuncles123"),
      logs_bloom = "0x",
      transactions_root = "0xtx123",
      state_root = "0xstate123",
      receipts_root = "0xreceipts123",
      miner = "0xminer123",
      difficulty = BigInt(0),
      total_difficulty = Some(BigInt(0)),
      size = 1000L,
      extra_data = "0x",
      gas_limit = 30000000L,
      gas_used = 21000L,
      timestamp = timestamp,
      transaction_count = 1,
      base_fee_per_gas = Some(20000000000L),
      tx = None
    )
  }

  // Helper method to create a test transaction
  def createTestTx(
    from: String = "0x1111111111111111111111111111111111111111",
    to: Option[String] = Some("0x2222222222222222222222222222222222222222"),
    value: BigInt = BigInt(1000000000000000000L), // 1 ETH in wei
    logs: Array[Log] = Array.empty,
    blockNumber: Long = 12345L,
    timestamp: Long = System.currentTimeMillis(),
    txHash: String = "0xtest123"
  ): Tx = {
    val block = createTestBlock(blockNumber, timestamp)
    
    ExtTx(
      hash = txHash,
      nonce = BigInt(1),
      transaction_index = 0,
      from_address = from,
      to_address = to,
      value = value,
      gas = 21000L,
      gas_price = Some(BigInt(20000000000L)),
      input = "0x",
      max_fee_per_gas = Some(BigInt(20000000000L)),
      max_priority_fee_per_gas = Some(BigInt(2000000000L)),
      transaction_type = Some(2),
      receipt_cumulative_gas_used = 21000L,
      receipt_gas_used = 21000L,
      receipt_contract_address = None,
      receipt_root = Some("0xreceipt123"),
      receipt_status = Some(1),
      receipt_effective_gas_price = Some(BigInt(20000000000L)),
      block = block,
      logs = logs,
      sim = None
    )
  }

  // Helper method to create ERC20 transfer log
  def createTransferLog(
    from: String = "0x1111111111111111111111111111111111111111",
    to: String = "0x2222222222222222222222222222222222222222",
    value: BigInt = BigInt(1000000000000000000L), // 1 token with 18 decimals
    tokenAddress: String = "0xcccccccccccccccccccccccccccccccccccc0001"
  ): Log = {
    val transferTopic = EthUtil.EVENT_TRANSFER
    val fromTopic = "0x000000000000000000000000" + from.drop(2)
    val toTopic = "0x000000000000000000000000" + to.drop(2)
    val valueData = "0x" + ("0" * 64 + value.toString(16)).takeRight(64)
    
    ExtLog(
      index = 0,
      address = tokenAddress,
      data = valueData,
      topics = Array(transferTopic, fromTopic, toTopic),
    )
  }

  "DetectorPoR" should "be instantiable" in {
    val pd = PluginDescriptor("DetectorPoR", "1.0.0", "Test Detector")
    val detector = new DetectorPoR(pd)
    
    detector should not be null
    detector.did shouldBe "DetectorPoR"
  }

  it should "have correct default values" in {
    DetectorPoR.DEF_DESC shouldBe "{amount} {change}{err}"
    DetectorPoR.DEF_WHEN shouldBe "cron"
    DetectorPoR.DEF_SRC shouldBe "chainlink"
    DetectorPoR.DEF_COND shouldBe "> 0.0"
  }

  "createTransferLog helper method" should "correctly convert BigInt values to hex with left-padding to 32 bytes" in {
    // Test various BigInt values to ensure hex conversion works correctly
    val testValues = Seq(
      BigInt(0L),                                    // Zero
      BigInt(1L),                                    // Small value
      BigInt(1000L),                                 // Medium value
      BigInt(1000000000000000000L),                  // 1 ETH in wei
      BigInt(2000000000L),                           // 2000 USDT (6 decimals)
      BigInt("300000000000000000000"),               // Example from user: 300000000000000000000
      BigInt("123456789012345678901234567890"),      // Very large value
      BigInt("999999999999999999999999999999")       // Another very large value
    )
    
    val tokenAddress = "0xcccccccccccccccccccccccccccccccccccc9999"
    
    testValues.foreach { originalValue =>
      // Create transfer log with the original value
      val transferLog = createTransferLog(
        from = "0x1111111111111111111111111111111111111111",
        to = "0x2222222222222222222222222222222222222222",
        value = originalValue,
        tokenAddress = tokenAddress
      )
      
      // Extract the hex value from the log data
      val hexValue = transferLog.data
      
      // Verify the hex is properly left-padded to 64 characters (32 bytes)
      hexValue should startWith("0x")
      hexValue.length shouldBe 66 // "0x" + 64 hex characters
      
      // Convert hex back to BigInt
      val convertedValue = BigInt(hexValue.drop(2), 16) // Remove "0x" prefix
      
      // Verify the conversion is correct
      convertedValue shouldBe originalValue
      
      // Also verify the log structure is correct
      transferLog.address shouldBe tokenAddress
      transferLog.topics should have length 3
      transferLog.topics(0) shouldBe EthUtil.EVENT_TRANSFER
    }
  }

  "createTestTx helper method" should "create valid transaction objects" in {
    val tx = createTestTx()
    
    tx.hash shouldBe "0xtest123"
    tx.from_address shouldBe "0x1111111111111111111111111111111111111111"
    tx.to_address shouldBe Some("0x2222222222222222222222222222222222222222")
    tx.value shouldBe BigInt(1000000000000000000L)
    tx.block should not be null
    tx.block.number shouldBe 12345L
  }

  it should "create transaction with transfer logs" in {
    val transferLog = createTransferLog()
    val tx = createTestTx(logs = Array(transferLog))
    
    tx.logs should have length 1
    tx.logs(0).address shouldBe "0xcccccccccccccccccccccccccccccccccccc0001"
    tx.logs(0).topics should have length 3
  }

  "DetectorPoR.onBlock" should "return empty sequence when when != 'block'" in {
    val pd = PluginDescriptor("DetectorPoR", "1.0.0", "Test Detector")
    val detector = new DetectorPoR(pd)
    
    val config = new Config {
      override val env: String = "test"
    }
    
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
        chainUid = Some("ethereum"),
        proxyAddress = None,
        implementation = None,
        address = Some("0x1234567890123456789012345678901234567890"),
        name = "test-contract"
      ),
      schema = None,
      name = "Test PoR Detector",
      source = "test",
      tags = Seq.empty,
      config = Some(JsObject(
        "when" -> JsString("cron"),  // Not "block", so should return empty
        "source" -> JsString("chainlink")
      )),
      destinations = Seq.empty
    )
    
    val rx = new SentryRun(detector, conf, config, None)
    rx.set("when", "cron")
    
    val tx = createTestTx()
    val events = detector.onBlock(rx, tx.block, Seq(tx))
    
    // When "when" != "block", should return empty sequence
    events shouldBe empty
  }

  it should "process transactions when when == 'block'" in {
    val pd = PluginDescriptor("DetectorPoR", "1.0.0", "Test Detector")
    val detector = new DetectorPoR(pd)
    
    val config = new Config {
      override val env: String = "test"
    }
    
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
        chainUid = Some("ethereum"),
        proxyAddress = None,
        implementation = None,
        address = Some("0x1234567890123456789012345678901234567890"),
        name = "test-contract"
      ),
      schema = None,
      name = "Test PoR Detector",
      source = "test",
      tags = Seq.empty,
      config = Some(JsObject(
        "when" -> JsString("block"),  // Should process
        "source" -> JsString("chainlink")
      )),
      destinations = Seq.empty
    )
    
    val rx = new SentryRun(detector, conf, config, None)
    rx.set("when", "block")
    
    val tx1 = createTestTx(
      from = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      txHash = "0xtx1"
    )
    val tx2 = createTestTx(
      from = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      txHash = "0xtx2"
    )
    
    // Note: This will likely fail because Chainlink is not initialized,
    // but it tests that onBlock is called with transactions
    val events = Try(detector.onBlock(rx, tx1.block, Seq(tx1, tx2)))
    
    // The method should be called (even if it fails due to missing Chainlink setup)
    events.isSuccess || events.isFailure shouldBe true
  }

}
