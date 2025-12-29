package io.syspulse.ext.sentinel

import scala.jdk.CollectionConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import com.typesafe.scalalogging.Logger
import scala.util.{Try, Success, Failure}

import io.syspulse.skel.plugin.{Plugin, PluginDescriptor}

import io.hacken.ext.core.Severity
import io.hacken.ext.sentinel.SentryRun
import io.hacken.ext.sentinel.Sentry
import io.hacken.ext.sentinel.util.EventUtil
import io.hacken.ext.detector.DetectorConfig
import io.hacken.ext.core.Event

import io.syspulse.ext.sentinel.feeds._
import io.syspulse.skel.util.Util
import io.syspulse.skel.script.{Script, ScriptFlow}
import io.hacken.ext.sentinel.ScriptEngine
import io.hacken.ext.sentinel.ThresholdDouble

object DetectorNews {  

}

class DetectorNews(pd: PluginDescriptor) extends DetectorFeed(pd) {
    
}
