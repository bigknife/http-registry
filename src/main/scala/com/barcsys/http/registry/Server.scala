package com.barcsys.http.registry

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.barcsys.http.registry.BizActors.Heartbeat.CheckAllMsg
import com.barcsys.http.registry.CommonActors.MemCache.{LoopLoadFromPersisted}
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
  val heartbeat = Heartbeat.createActor

  //// store
  implicit lazy val connectionString = {
    val str = Option(System.getenv("HTTP_REGISTY_MONGO"))
      .getOrElse(Option(System.getProperty("httpRegistryMongo"))
        .getOrElse("mongo://localhost:27017/http_registry"))

    ConnectionString(str).get
  }
  implicit lazy val dbName = connectionString.path.getOrElse("http_registry").dropWhile(_ == "/")
  implicit lazy val mongoClient = Store.buildMongoClient

  import scala.concurrent.ExecutionContext.Implicits.global

  val store = PersistedStore.createActor

  def start(interface: String, port: Int) = {
    import scala.concurrent.duration._

    memCache ! LoopLoadFromPersisted(5.seconds)
    heartbeat ! CheckAllMsg(10.seconds)

    IO(Http) ! Http.Bind(listener, interface, port)
  }

  def stop = {
    system.terminate()
  }
}
