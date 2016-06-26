package com.barcsys.http.registry

import akka.actor.{Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props}
import com.barcsys.http.registry.CommonActors.{MemCache, PersistedStore}
import com.barcsys.http.registry.Types.{Method, ServiceInstance}
import spray.can.Http
import spray.http._
import spray.http.StatusCodes._
import spray.http.HttpMethods._
import spray.json._
import akka.pattern.ask
import akka.util.Timeout
import com.barcsys.http.registry.BizActors.ServicesRegister.ServiceRegistration
import spray.http._
import spray.client.pipelining._

import scala.concurrent.duration._
import com.barcsys.http.registry.CommonActors.MemCache.{GetMsg, PutMsg}
import com.barcsys.http.registry.CommonActors.PersistedStore.SaveServiceInstanceMsg

import scala.concurrent.{Await, Future}
import scala.util.Try

/**
  * Created by bigknife on 16/6/23.
  */
object BizActors {

  import BaseActors._

  class HttpListener extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {

    import com.barcsys.http.registry.BizActors.ServicesRegister._

    lazy val serviceRegister = ServicesRegister.createChildActorRef

    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case _: Http.Connected => sender ! Http.Register(self)

      case HttpRequest(POST, Uri.Path("/v1/services"), _, entity, _) =>
        val client = sender
        serviceRegister ! ServiceRegistration(client, entity)

      case x: HttpRequest => sender ! HttpResponse(status = NotFound, entity = "Resource Not Found")
    }
  }

  class ServicesRegister extends
    Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {

    def wrappedReceive: Receive = {
      case ServiceRegistration(client, entity) =>
        // 写入缓存即返回
        // then write to persisted store
        import Protocol.ServiceProtocol._
        //反序列化为对象
        Try{
          entity.asString.parseJson.convertTo[ServiceInstance]
        }match{
          case scala.util.Success(x) =>
            client ! HttpResponse(status = OK)
            //set uid, service instance uid is serviceId + serviceVersion + baseUrl
            //service uid is id + version
            val optSuidAndSIuid = for{
              s <- x.service
              sId <- s.id
              sVer <- s.version
              siBaseUrl <- x.baseUrl
            } yield (sId + ":" + sVer, sId + ":" + sVer + ":" + siBaseUrl)

            val s = x.service.map(s0 => s0.copy(uid = optSuidAndSIuid.map(_._1)))
            val si = x.copy(uid = optSuidAndSIuid.map(_._2), service = s, upTime = Some(System.currentTimeMillis()))

            MemCache.selectActorFromContext ! PutMsg(si.uid.get, si)
            PersistedStore.selectActorFromContext ! SaveServiceInstanceMsg(si)

          case scala.util.Failure(t) =>
            client ! HttpResponse(status = BadRequest)
            logger.error("bad request, the ServiceInstanceDiscovery protocol expected", t)
        }

    }
  }

  object ServicesRegister {

    case class ServiceRegistration(sender: ActorRef, entity: HttpEntity)

    def createChildActorRef(implicit context: ActorContext): ActorRef = {
      context.actorOf(Props[ServicesRegister], "services-handler")
    }
  }
}
