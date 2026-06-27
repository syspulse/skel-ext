package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration,FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try,Success,Failure}

import io.syspulse.skel.plugin.{Plugin,PluginDescriptor}

import io.syspulse.skel.blockchain.Blockchain
import io.syspulse.skel.crypto.eth.protocols.chainlink.Chainlink
import io.syspulse.skel.crypto.eth.SolidityTuple
import io.syspulse.skel.crypto.eth.Web3jTrace
import io.syspulse.skel.crypto.Eth
import io.syspulse.skel.crypto.eth.TokenUtil

import io.haas.ingest.eth.flow.etl.{Block,Tx}
import io.haas.ingest.eth.{MempoolTx}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.SentinelBlockchains._
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig

import io.hacken.ext.sentinel.util.TokenData
import io.hacken.ext.core.Event
import io.hacken.ext.sentinel.ThresholdDouble
import io.hacken.ext.sentinel.WithEvm


// --------------------------------------------------------------------------------------------------------------------------
object DetectorPoR {
  val DEF_DESC = "{amount} {change}{err}"
  val DEF_WHEN = "cron"
  val DEF_SRC = "chainlink"
  val DEF_COND = "> 0.0"

  val DEF_TRACK_ERR = true  
  val DEF_TRACK_ERR_ALWAYS = true

  val DEF_SEV_OK = Severity.INFO
  val DEF_SEV_WARN = Severity.MEDIUM
  val DEF_SEV_ERR = Severity.ERROR

  val WHEN_CRON = "cron"
  val WHEN_BLOCK = "block"
}

class DetectorPoR(pd: PluginDescriptor) extends WithEvm[Tx,Block,MempoolTx] with SentryEth with Plugin {  
  override def did = pd.name
  override def toString = s"${this.getClass.getSimpleName}(${did})"

  override def getSettings(rx:SentryRunEth): Map[String,Any] = {
    rx.getConfig().env match {
      case "test" => Map()
      case "dev" =>
        Map( "_cron_rate_limit" -> 1 * 10 * 1000L ) // 10 sec
      case _ =>
        Map( "_cron_rate_limit" -> 1 * 60 * 1000L ) // 1 min
    }
  }
    
  override def onInit(rx:SentryRunEth,conf: DetectorConfig): Int = {    

    //onUpdate(rx,conf)
    val r = super.onInit(rx,conf)
    if(r != SentryRun.SENTRY_INIT) {
      return r
    }

    // init Aave
    val contractsConfig = rx.getConfiguration()(c => Some(c.getListString("chainlink.contracts"))).getOrElse(Seq.empty)
    val chainlink = Chainlink(contractsConfig)

    if(chainlink.size() == 0) {
      log.warn(s"${rx.getExtId()}: Oracle Contracts undefined: ${chainlink}")
      error("Oracle contracts undefined",None)
      return SentryRun.SENTRY_STOPPED
    }

    rx.set("chainlink",chainlink)

    SentryRun.SENTRY_INIT
  }

  override def onStart(rx:SentryRunEth,conf:DetectorConfig):Int = {    

    val r = onUpdate(rx,conf)
    if(r != SentryRun.SENTRY_RUNNING) {
      return r
    }

    super.onStart(rx,conf)
  }

  override def onUpdate(rx:SentryRunEth,conf: DetectorConfig): Int = {    

    if(rx.isAddrEmpty()) {
      log.warn(s"${rx.getExtId()}: address undefined: ${rx.getAddr()}")
      error(s"Address undefined",None)
      return SentryRun.SENTRY_STOPPED
    }
    
    // val apiKey = {
    //   val key = DetectorConfig.getString(conf,"api_key","")
    //   if(key.isEmpty()) {
    //     sys.env.get("CG_API_KEY").getOrElse("")
    //   } else 
    //     key      
    // }

    // rx.set("api_key",apiKey)

    // set state    
    rx.set("desc",DetectorConfig.getString(rx.conf,"desc",DetectorPoR.DEF_DESC))

    val source = DetectorConfig.getString(rx.conf,"source",DetectorPoR.DEF_SRC)

    // check we have Oracle for address
    val por0 = if(source == "chainlink" && rx.getChain().isDefined) {
      val chainlink = rx.get("chainlink").get.asInstanceOf[Chainlink]
      val por = chainlink.getPoR(rx.getChain().get,rx.getAddr().get)(getWeb3(rx).get)
      if(por.isFailure) {
        log.warn(s"${rx.getExtId()}: Oracle not found: ${rx.getAddr().get}: ${por}")
        error(s"Oracle not found for address: ${rx.getAddr().get}",None)
        return SentryRun.SENTRY_STOPPED
      }

      por.get

    } else 0.0

    rx.set("condition",new ThresholdDouble(por0,DetectorConfig.getString(rx.conf,"condition",DetectorPoR.DEF_COND))) 
    rx.set("source",source)
    rx.set("when",DetectorConfig.getString(rx.conf,"when",DetectorPoR.DEF_WHEN))
    // rx.set("currency",DetectorConfig.getString(rx.conf,"currency","usd"))

    // set initial prices in condition, ignore output
    getPoR(rx,None)
    
    SentryRun.SENTRY_RUNNING
  }

  def getPoR(rx:SentryRunEth,addr:Option[String]):Seq[Event] = {
    val addr = rx.getAddr() match {
      case Some(addr) => addr.toLowerCase
      case None => 
        log.warn(s"${rx.getExtId()}: address undefined")
        return Seq.empty
    }

    //val apiKey = rx.get("api_key").asInstanceOf[Option[String]].getOrElse("")
    val source = rx.get("source").asInstanceOf[Option[String]].getOrElse(DetectorPoR.DEF_SRC)
    val chainlink = rx.get("chainlink").get.asInstanceOf[Chainlink]
    //val currency = rx.get("currency").asInstanceOf[Option[String]].getOrElse("usd")

    val r = source match {
      case "chainlink" if(rx.getChain().isDefined) => chainlink.getPoR(rx.getChain().get,addr)(getWeb3(rx).get)
      case "chainlink" => 
        Failure(new Exception(s"chain not defined"))
      case _ => 
        Failure(new Exception(s"invalid source: '${source}'"))
    }
    
    val ee = r match {
      case Success(por) =>
        val condition = rx.get("condition").get.asInstanceOf[ThresholdDouble]
        val por0 = condition.value()
                
        val por1 = por        
        val changed = condition.set(por1)

        log.info(s"${rx.getExtId()}: addr=${addr}: ${por0} -> ${por1}: [${condition}: changed=${changed}]")

        //if(por >= condition ) 
        if(changed) {
          val desc = DetectorConfig.getString(rx.conf,"desc",DetectorPoR.DEF_DESC)

          val diff = por - por0
          val diffPercentHuman = if(por0 != 0) 
            s"${TokenUtil.toHuman(( diff / por0 ) * 100.0)}%" 
          else 
            ""

          val amount = TokenUtil.toHuman(por)

          Seq(EventUtil.createEvent(
            did,tx = None,Some(addr),conf = Some(rx.getConf()),
            meta = Map(
              "amount" -> amount,
              "por"-> por.toString,
              "old"-> por0.toString,
              "diff"-> s"${diff.toString}",
              "change" -> diffPercentHuman,
              //"currency" -> currency,
              "desc"-> desc
            ),
            sev = Some(DetectorPoR.DEF_SEV_WARN)
          ))
        } else
          Seq.empty
      case Failure(e) =>
        //log.error(s"Detector(${rx.getProjectId()}/${rx.getId()}):",e)
        log.warn(s"${rx.getExtId()}: ${e.getMessage()}")
        Seq.empty
    }
    
    ee
  }

  override def onCron(rx:SentryRunEth,elapsed:Long):Seq[Event] = {

    val when = rx.get("when").asInstanceOf[Option[String]].getOrElse("")
    if(when != "cron") {
      return Seq.empty
    }

    getPoR(rx,None)
  }
  
  override def onBlock(rx:SentryRunEth,block:Block,txx:Seq[Tx]):Seq[Event] = {
    val when = rx.get("when").asInstanceOf[Option[String]].getOrElse("")
    if(when != "block") {
      return Seq.empty
    }
    getPoR(rx,None)
  }

}

