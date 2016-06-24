package com.barcsys.http.registry

import akka.actor.{Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props}
import com.barcsys.http.registry.BaseActors.{ActorMessageTrack, ActorMetrics, Slf4jLogging}
import com.barcsys.http.registry.CommonActors.MemCache.{GetMsg, MemCacheMsg, PutMsg}

/**
  * Created by bigknife on 16/6/24.
  */
object CommonActors {

  var cache:Map[String, Any] = Map.empty

  class MemCache extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {
    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case PutMsg(k, v) => cache = cache + (k -> v)
      case GetMsg(k) => sender ! cache.get(k)
      case _ =>
    }
  }

  object MemCache {

    sealed trait MemCacheMsg

    case class PutMsg(key: String, value: Any) extends MemCacheMsg

    case class GetMsg(key: String) extends MemCacheMsg

    case class DelMsg(key: String) extends MemCacheMsg


    def createActor(implicit system: ActorSystem): ActorRef = {
      system.actorOf(Props[MemCache], "mem-cache")
    }

    def selectActorFromSystem(implicit system: ActorSystem): ActorSelection = {
      system.actorSelection("/user/mem-cache")
    }

    def selectActorFromContext(implicit context: ActorContext): ActorSelection = {
      context.actorSelection("/user/mem-cache")
    }
  }
}
