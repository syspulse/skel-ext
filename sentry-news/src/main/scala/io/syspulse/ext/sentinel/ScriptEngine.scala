package io.hacken.ext.sentinel

import scala.util.Random
import io.jvm.uuid._
import com.typesafe.scalalogging.Logger
import scala.util.{Success,Failure,Try}

import io.syspulse.skel.script.{Script, ScriptFlow}

import io.hacken.ext.detector.{DetectorConfig}
import io.hacken.ext.sentinel.SentryRun

// Main functionality is in skel
// This engine allows to create based on Configuration
object ScriptEngine {
  val log = Logger(s"${this.getClass()}")

  def loadConfig(rx:SentryRun,conf: DetectorConfig, defTrackErr: Boolean, defTrackErrAlways: Boolean):Int = {

    val scripts:Seq[Script] = (conf.config.map(conf => {
      val json = ujson.read(conf.toString())

      json.obj.get("script").map { scriptArray =>
        scriptArray.arr.flatMap(j => {
          val scriptType = j.obj("type").str
          val scriptSrc = j.obj("src").str
          val scriptOpt = j.obj.get("options").map(_.str).getOrElse("")

          ScriptFlow.parseUri(scriptType + "://" + scriptSrc) match {
            case Success(script) => Some(script)
            case Failure(e) =>
              log.warn(s"${rx.getExtId()}: Failed to load script: '${scriptType}://${scriptSrc}': ${e.getMessage()}")
              None
          }
        }).toSeq
      }.getOrElse(Seq.empty)
    }).getOrElse(Seq.empty))

    // Store Seq[Script] directly for DetectorNews compatibility
    val scriptFlow = new ScriptFlow(scripts)
    rx.set("scripts", scriptFlow)

    rx.set("track_err",DetectorConfig.getBoolean(rx.conf,"track_err",defTrackErr))
    rx.set("err_always",DetectorConfig.getBoolean(rx.conf,"err_always",defTrackErrAlways))

    SentryRun.SENTRY_RUNNING
  }
  
}
