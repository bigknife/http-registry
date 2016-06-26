package com.barcsys.http.registry

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props}
import com.barcsys.http.registry.BaseActors.{ActorMessageTrack, ActorMetrics, Slf4jLogging}
import com.barcsys.http.registry.CommonActors.MemCache.{GetMsg, PutMsg}
import com.barcsys.http.registry.CommonActors.PersistedStore.{SaveServiceInstanceMsg}
import com.barcsys.http.registry.Types.{ServiceInstance}
import org.mongodb.scala.MongoClient

import scala.concurrent.ExecutionContext

/**
  * Created by bigknife on 16/6/24.
  */
object CommonActors {

  //var cache:Map[String, Any] = Map.empty
  val cache: AtomicReference[Map[String, Any]] = new AtomicReference[Map[String, Any]](Map.empty)

  /**
    * MemCache Actor
    */
  class MemCache extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {
    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理

    def wrappedReceive: Receive = {
      case PutMsg(k, v) => cache.set(cache.get() + (k -> v))
      case GetMsg(k) => sender ! Option(cache.get().get(k))
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

  class PersistedStore(mongoClient: MongoClient,
                       dbName: String,
                       ec: ExecutionContext)
    extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {

    implicit private val _mongoClient = mongoClient
    implicit private val _dbName = dbName
    implicit private val _ec = ec

    import Store.Implicits.serviceInstance2Document

    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case SaveServiceInstanceMsg(x) =>
        Store.saveServiceInstance(x)

      case _ =>
    }
  }

  object PersistedStore {

    case class SaveServiceInstanceMsg(serviceInstance: ServiceInstance)

    def createActor(implicit system: ActorSystem, mongoClient: MongoClient, dbName: String, ec: ExecutionContext): ActorRef = {
      val props = Props(classOf[PersistedStore], mongoClient, dbName, ec)
      system.actorOf(props, "persisted-store")
    }

    def selectActorFromContext(implicit context: ActorContext): ActorSelection = {
      context.actorSelection("/user/persisted-store")
    }
  }
}
