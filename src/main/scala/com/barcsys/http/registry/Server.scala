package com.barcsys.http.registry

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.barcsys.http.registry.CommonActors.{MemCache, PersistedStore}
import com.barcsys.http.registry.Misc.ConnectionString
import spray.can.Http

/**
  * Created by bigknife on 16/6/23.
  */
object Server {

  import BizActors._

  implicit val system = ActorSystem("http-registry")
  val listener = system.actorOf(Props[HttpListener], "http-listener")
  val memCache = MemCache.createActor

  //// store
  implicit lazy val connectionString = ConnectionString("mongo://localhost:27017/http_registry").get
  implicit lazy val dbName = connectionString.path.getOrElse("http_registry").dropWhile(_ == "/")
  implicit lazy val mongoClient = Store.buildMongoClient
  import scala.concurrent.ExecutionContext.Implicits.global

  val store = PersistedStore.createActor

  def start(interface: String, port: Int) = {
    IO(Http) ! Http.Bind(listener, interface, port)
  }

  def stop = {
    system.terminate()
  }
}
