package com.barcsys.http.registry

import akka.actor.ActorSystem
import akka.util.Timeout
import com.barcsys.http.registry.CommonActors.MemCache
import com.barcsys.http.registry.CommonActors.MemCache.{GetMsg, PutMsg}

import scala.concurrent.duration._
import akka.pattern.ask

import scala.concurrent.Await


/**
  * Created by bigknife on 16/6/24.
  */
object CommonActorsTest extends App{

  implicit val system = ActorSystem()
  implicit val timeout = Timeout(10.seconds)
  import system.dispatcher

  MemCache.createActor

  val cache = MemCache.selectActorFromSystem


  cache ! PutMsg("test_key", "Hello,World")

  val f = cache ? GetMsg("test_key")

  val t = Await.result(f, 10.seconds)

  println("t = " + t)

  system.terminate()

}
