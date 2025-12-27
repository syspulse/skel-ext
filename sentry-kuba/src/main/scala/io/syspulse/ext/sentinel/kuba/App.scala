package io.syspulse.ext.sentinel.kuba

import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.Await

import io.syspulse.skel
import io.syspulse.skel.util.Util
import io.syspulse.skel.config._
import io.syspulse.skel.auth.jwt.AuthJwt

import io.jvm.uuid._

import io.syspulse.skel.FutureAwaitable._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

import java.util.Base64
import akka.actor.typed.ActorSystem

case class Config(
  host:String="0.0.0.0",
  port:Int=8080,
  uri:String = "/api/v1/aml",

  datastore:String = "mem://",

  duneApiKey:String = "",
  output:String = "output.csv",

  cmd:String = "dune",
  params: Seq[String] = Seq(),
)

object App extends skel.Server {

  def main(args:Array[String]):Unit = {
    Console.err.println(s"args: '${args.mkString(",")}'")    

    val d = Config()
    val c = Configuration.withPriority(Seq(
      new ConfigurationAkka,
      new ConfigurationProp,
      new ConfigurationEnv,
      new ConfigurationArgs(args,"sentry-aml","",
        ArgString('h', "http.host",s"listen host (def: ${d.host})"),
        ArgInt('p', "http.port",s"listern port (def: ${d.port})"),
        ArgString('u', "http.uri",s"api uri (def: ${d.uri})"),

        ArgString('d', "datastore",s"Datastore [] (def: ${d.datastore})"),

        ArgString('_', "dune.api.key",s"Dune API Key (def: ${d.duneApiKey})"),
        ArgString('o', "output",s"Output file (def: ${d.output})"),

        ArgCmd("dune","Dune"),
        ArgCmd("dune-fetch","Fetch Dune query results"),

        ArgParam("<params>",""),
        ArgLogging()
      ).withExit(1)
    )).withLogging()

    implicit val config = Config(
      host = c.getString("http.host").getOrElse(d.host),
      port = c.getInt("http.port").getOrElse(d.port),
      uri = c.getString("http.uri").getOrElse(d.uri),

      datastore = c.getString("datastore").getOrElse(d.datastore),

      duneApiKey = c.getString("dune.api.key").getOrElse(d.duneApiKey),
      output = c.getString("output").getOrElse(d.output),

      cmd = c.getCmd().getOrElse(d.cmd),
      params = c.getParams(),
    )

    // ATTENTION: WORKAROUND for running Sentinel Detector and ext-aml from the same Docker image
    if(config.cmd == "sentinel") {
      io.hacken.ext.sentinel.App.main(args)      
    } else {

    Console.err.println(s"Config: ${config}")    

      
    val r = config.cmd match {
      case "dune-fetch" =>
        // Validate parameters
        if (config.params.isEmpty) {
          Console.err.println("Error: queryId parameter is required")
          Console.err.println("Usage: dune-fetch <queryId>")
          System.exit(1)
        }

        if (config.duneApiKey.isEmpty) {
          Console.err.println("Error: dune.api.key is required")
          Console.err.println("Set it via --dune.api.key or DUNE_API_KEY environment variable")
          System.exit(1)
        }

        val queryId = config.params.head
        Console.err.println(s"Fetching Dune query ${queryId} to ${config.output}")

        import io.syspulse.ext.sentinel.kuba.api.DuneApi

        DuneApi.fetchAllResults(queryId, config.duneApiKey, config.output) match {
          case scala.util.Success(linesWritten) =>
            Console.err.println(s"Successfully wrote ${linesWritten} lines to ${config.output}")
            linesWritten
          case scala.util.Failure(e) =>
            Console.err.println(s"Error fetching Dune query: ${e.getMessage}")
            e.printStackTrace()
            System.exit(1)
        }

      case "dune" =>
        Console.err.println("Available commands: dune-fetch")
        0
    }
    Console.err.println(s"r = ${r}")

  }}
}
