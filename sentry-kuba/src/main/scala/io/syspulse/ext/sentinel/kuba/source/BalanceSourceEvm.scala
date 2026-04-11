package io.syspulse.ext.sentinel.kuba.source

import scala.util.{Try, Success, Failure}
import com.typesafe.scalalogging.Logger

import io.syspulse.skel.blockchain.Token
import io.syspulse.skel.crypto.eth.Web3jTrace
import io.syspulse.skel.crypto.Eth
import io.syspulse.skel.crypto.eth.TokenUtil
import io.syspulse.ext.sentinel.Balance
import io.syspulse.ext.sentinel.kuba.api.Moralis

// EVM-specific implementation
class BalanceSourceEvm(rpcUrl: String, chainName: String, chainId: String, snapshotTs: Long, moralisApiKey: String) extends BalanceSource {

  private val log = Logger(getClass.getName)
  private val web3Trace: Web3jTrace = Eth.web3(rpcUrl)

  // Calculate and cache block for the snapshot timestamp
  private val cachedBlock: Long = {
    if (!moralisApiKey.isBlank) {
      // Use Moralis API to get block by timestamp
      Moralis.getBlockByTimestamp(chainName, snapshotTs, moralisApiKey) match {
        case Success(blockNumber) =>
          log.info(s"Got block ${blockNumber} for chain ${chainName} (${chainId}) at timestamp ${snapshotTs}")
          blockNumber
        case Failure(e) =>
          log.warn(s"Failed to get block from Moralis API for chain ${chainName}: ${e.getMessage}, falling back to current block")
          web3Trace.ethBlockNumber().send().getBlockNumber.longValue()
      }
    } else {
      log.warn(s"Moralis API key not configured, using current block for chain ${chainName}")
      web3Trace.ethBlockNumber().send().getBlockNumber.longValue()
    }
  }

  def getBalance(addr:String,chain:String,token:Token,block:Option[Long]):Try[Balance] = {
    val actualBlock = block.getOrElse(cachedBlock)

    Try {
      val isNativeToken = token.addr.isEmpty

      val balanceWei: BigInt = if (isNativeToken) {
        // Native token (ETH) balance
        // val blockParam = org.web3j.protocol.core.DefaultBlockParameter.valueOf(java.math.BigInteger.valueOf(actualBlock))
        // val ethBalance = web3Trace.ethGetBalance(addr, blockParam).send().getBalance
        // BigInt(ethBalance)
        Eth.getBalance(addr,Some(actualBlock))(web3Trace) match {
          case Success(balance) => balance
          case Failure(e) => throw e
        }
      } else {
        // ERC20 token balance
        TokenUtil.askErc20Balance(token.addr, addr, Some(token.dec), Some(actualBlock))(web3Trace) match {
          case Success(bal) => bal
          case Failure(e) => throw e
        }
      }

      // Convert from wei/token units to decimal based on token decimals
      val value = balanceWei
      val amount = BigDecimal(value) / BigDecimal(10).pow(token.dec)

      Balance(addr, chain, token.sym, token.addr, value, amount.toDouble, Some(actualBlock), snapshotTs, None)
    }.recoverWith {
      case e: Exception =>
        Failure(new Exception(s"Failed to get EVM balance for ${addr}/${token.sym} at block ${actualBlock}: ${e.getMessage}"))
    }
  }

  override def getBlocks(): Map[String, Long] = Map(chainName -> cachedBlock)
}
