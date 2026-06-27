package io.syspulse.ext.sentinel.kuba.source

import scala.util.{Try, Failure}
import com.typesafe.scalalogging.Logger

import io.syspulse.skel.blockchain.Token
import io.syspulse.skel.blockchain.BlockchainRpc
import io.syspulse.ext.sentinel.Balance
import io.syspulse.ext.sentinel.DetectorKuba

// Router that delegates to appropriate chain-specific implementation
class BalanceSourceChain(rpc: Map[String,BlockchainRpc], snapshotTs: Long, moralisApiKey: String) extends BalanceSource {
  val log = Logger(this.getClass.getSimpleName)

  // Map chain_id -> BalanceSource implementation
  // Each source calculates and caches its block height during construction
  private val chainSources: Map[String, BalanceSource] = {
    rpc.flatMap { case (chainName, r) =>
      val name = r.name
      val id = r.id
      val rpcUrl = r.rpcUri

      name match {
        case _ if DetectorKuba.isEvmChain(name) =>
          Some(name -> new BalanceSourceEvm(rpcUrl, name, id, snapshotTs, moralisApiKey))
        case "sol" | "solana" =>
          Some(name -> new BalanceSourceSolana(rpcUrl, name, snapshotTs))
        case _ =>
          log.warn(s"unsupported chain: ${name} (${id})")
          None
      }
    }
    .toMap
  }

  def getBalance(addr:String,chain:String,token:Token,block:Option[Long]):Try[Balance] = {
    chainSources.get(chain.toLowerCase) match {
      case Some(source) => source.getBalance(addr, chain, token, block)
      case None => Failure(new Exception(s"No balance source configured for chain: ${chain}"))
    }
  }

  override def getBlocks(): Map[String, Long] = {
    chainSources.flatMap { case (chainName, source) =>
      source.getBlocks().map { case (name, block) =>
        (name, block)
      }
    }
  }
}
