package io.syspulse.ext.sentinel.news

import io.syspulse.skel
import io.syspulse.skel.util.Util
import io.syspulse.skel.config._

import io.jvm.uuid._

object App extends skel.Server {

  def main(args:Array[String]):Unit = {
    Console.err.println(s"args: '${args.mkString(",")}'")    

    Console.err.println(s"PWD: ${os.pwd}")
  }
}
