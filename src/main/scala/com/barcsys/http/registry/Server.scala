package com.barcsys.http.registry

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.barcsys.http.registry.CommonActors.MemCache
import spray.can.Http

/**
  * Created by bigknife on 16/6/23.
  */
object Server {
  import BizActors._

  implicit val system = ActorSystem("http-registry")
  val listener = system.actorOf(Props[HttpListener], "http-listener")
  val heartbeatCheck = HeartbeatCheck.createActor
  val memCache = MemCache.createActor

  def start(interface: String, port: Int) = {
    IO(Http) ! Http.Bind(listener, interface, port)
  }

  def stop = {
    system.terminate()
  }
}
