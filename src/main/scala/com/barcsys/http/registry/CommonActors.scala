package com.barcsys.http.registry

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props}
import akka.util.Timeout
import com.barcsys.http.registry.BaseActors.{ActorMessageTrack, ActorMetrics, Slf4jLogging}
import com.barcsys.http.registry.CommonActors.MemCache._
import com.barcsys.http.registry.CommonActors.PersistedStore.{FilterServiceInstanceMsg, GetAllEnabledServiceInstancesMsg, SaveServiceInstanceMsg, UpdateServiceInstanceMsg}
import com.barcsys.http.registry.Types.{ServiceInstance, ServiceInstanceFilter}
import org.mongodb.scala.MongoClient

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import akka.pattern.ask

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
    implicit val timeout = Timeout(10.seconds)

    def wrappedReceive: Receive = {
      case PutMsg(k, v) => cache.set(cache.get() + (k -> v))
      case GetMsg(k) => sender ! Option(cache.get().get(k))
      case GetAll(clazz) => sender ! cache.get().filter(x => x._2.getClass == clazz)
      case LoopLoadFromPersisted(duration) =>
        import context.dispatcher
        (PersistedStore.selectActorFromContext ? GetAllEnabledServiceInstancesMsg).map {
          case x: Vector[_] => x.foreach {
            case si: ServiceInstance =>
              cache.set(cache.get() + (si.uid.get -> si))
          }
        }

        // 循环发送加载消息
        val _self = self
        Future {
          Thread.sleep(duration.toMillis)
          _self ! LoopLoadFromPersisted(duration)
        }

      case _ =>
    }
  }

  object MemCache {

    sealed trait MemCacheMsg

    case class PutMsg(key: String, value: Any) extends MemCacheMsg

    case class GetMsg(key: String) extends MemCacheMsg

    case class DelMsg(key: String) extends MemCacheMsg

    case class GetAll(clazz: Class[_]) extends MemCacheMsg

    case class LoopLoadFromPersisted(duration: Duration) extends MemCacheMsg


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
    import Store.Implicits.documentToServiceInstance

    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case SaveServiceInstanceMsg(x) =>
        Store.saveServiceInstance(x)

      case GetAllEnabledServiceInstancesMsg =>
        val _sender = sender
        Store.findEnabledServiceInstances.foreach(_sender ! _)

      case UpdateServiceInstanceMsg(x) =>
        Store.updateServiceInstance(x)

      case FilterServiceInstanceMsg(filter) =>
        //todo 待完成
        val doc = Store.Implicits.serviceInstanceFilterToDocument(filter)
        val _sender = sender
        Store.findServiceInstances(Some(doc)).onSuccess {
          case x: Vector[ServiceInstance] => _sender ! x
        }
    }
  }

  object PersistedStore {

    case object GetAllEnabledServiceInstancesMsg

    case class SaveServiceInstanceMsg(serviceInstance: ServiceInstance)

    case class UpdateServiceInstanceMsg(serviceInstance: ServiceInstance)

    case class FilterServiceInstanceMsg(filter: ServiceInstanceFilter)

    def createActor(implicit system: ActorSystem, mongoClient: MongoClient, dbName: String, ec: ExecutionContext): ActorRef = {
      val props = Props(classOf[PersistedStore], mongoClient, dbName, ec)
      system.actorOf(props, "persisted-store")
    }

    def selectActorFromContext(implicit context: ActorContext): ActorSelection = {
      context.actorSelection("/user/persisted-store")
    }
  }
}
