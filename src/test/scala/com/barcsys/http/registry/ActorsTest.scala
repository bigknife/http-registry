package com.barcsys.http.registry

import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout
import com.barcsys.http.registry.BaseActors.ActorStack

import scala.concurrent.Await

/**
  * Created by bigknife on 16/6/23.
  */
object ActorsTest extends App{
  trait Aspect1 extends ActorStack {
    override def receive: Receive = {
      case x =>
        println("This is Aspect1 start")
        super.receive(x)
        println("This is Aspect1 end")
    }
  }
  trait Aspect2 extends ActorStack {
    override def receive: Receive = {
      case x =>
        println("This is Aspect2 start")
        super.receive(x)
        println("This is Aspect2 end")
    }
  }

  class DummyActor extends Actor with Aspect1 with Aspect2 {
    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case x =>
        println("This is dummy actor doing")
        sender ! None
    }
  }

  implicit  val system = ActorSystem()
  val dummyActor = system.actorOf(Props[DummyActor])
  import scala.concurrent.duration._
  implicit val timeout = Timeout(10.seconds)
  import akka.pattern.ask
  val f = dummyActor ? None
  Await.result(f, atMost = 10.seconds)
  system.terminate()
}
