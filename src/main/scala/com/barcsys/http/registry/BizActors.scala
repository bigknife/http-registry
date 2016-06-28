package com.barcsys.http.registry

import akka.actor.{Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props}
import com.barcsys.http.registry.CommonActors.{MemCache, PersistedStore}
import com.barcsys.http.registry.Types.{ServiceInstance, ServiceInstanceFilter}
import spray.can.Http
import spray.http.StatusCodes._
import spray.http.HttpMethods._
import spray.json._
import akka.pattern.ask
import akka.util.Timeout
import com.barcsys.http.registry.BizActors.Heartbeat.CheckAllMsg
import com.barcsys.http.registry.BizActors.ServiceInstanceFilterActor.QueryServiceInstanceMsg
import com.barcsys.http.registry.BizActors.ServicesRegister.ServiceRegistration
import spray.http._
import spray.client.pipelining._

import scala.concurrent.duration._
import com.barcsys.http.registry.CommonActors.MemCache.{GetAll, GetMsg, PutMsg}
import com.barcsys.http.registry.CommonActors.PersistedStore.{FilterServiceInstanceMsg, SaveServiceInstanceMsg, UpdateServiceInstanceMsg}
import com.barcsys.http.registry.Types.ServiceInstanceStatus.{Lost, Running, Unavailable, Waiting}
import com.barcsys.http.registry.Types.ServiceStatus.Enabled

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * Created by bigknife on 16/6/23.
  */
object BizActors {

  import BaseActors._

  class HttpListener extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {

    import com.barcsys.http.registry.BizActors.ServicesRegister._

    lazy val serviceRegister = ServicesRegister.createChildActorRef
    lazy val serviceInstanceFilter = ServiceInstanceFilterActor.createChildActor

    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case _: Http.Connected => sender ! Http.Register(self)

      case HttpRequest(_, Uri.Path("/v1"), _, _, _ ) =>
        sender ! HttpResponse(entity = "http-registry/v1:0.1.0.0-SNAPSHOT")

      case HttpRequest(POST, Uri.Path("/v1/services"), _, entity, _) =>
        val client = sender
        serviceRegister ! ServiceRegistration(client, entity)

      case HttpRequest(POST, Uri.Path("/v1/services/filter"), _, entity, _) =>
        val client = sender
        serviceInstanceFilter ! QueryServiceInstanceMsg(client, entity)

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
        Try {
          entity.asString.parseJson.convertTo[ServiceInstance]
        } match {
          case scala.util.Success(x) =>
            client ! HttpResponse(status = OK)
            //set uid, service instance uid is serviceId + serviceVersion + baseUrl
            //service uid is id + version
            val optSuidAndSIuid = for {
              s <- x.service
              sId <- s.id
              sVer <- s.version
              siBaseUrl <- x.baseUrl
            } yield (sId + ":" + sVer, sId + ":" + sVer + ":" + siBaseUrl)

            val s = x.service.map(s0 => s0.copy(
              uid = optSuidAndSIuid.map(_._1),
              status = Some(Enabled))
            )

            val si = x.copy(
              uid = optSuidAndSIuid.map(_._2),
              service = s,
              upTime = Some(System.currentTimeMillis()),
              status = Some(Waiting)
            )

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

  class Heartbeat extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {
    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case CheckAllMsg(duration) =>
        implicit val timeout = Timeout(1.seconds)
        import context.dispatcher

        val opt = MemCache.selectActorFromContext ? GetAll(classOf[ServiceInstance])
        opt.onSuccess {
          case x: Map[_, _] => x.foreach {
            case (k, v) =>
              val si = v.asInstanceOf[ServiceInstance]
              val future = checkHeartbeat(si) {
                case HttpResponse(status, _, _, _) => status.intValue match {
                  case s if s < 300 && s >= 200 =>
                    //服务上线
                    val nsi = si.copy(status = Some(Running))
                    //update nsi
                    MemCache.selectActorFromContext ! PutMsg(nsi.uid.get, nsi)
                    PersistedStore.selectActorFromContext ! UpdateServiceInstanceMsg(nsi)

                  case s if s >= 300 =>
                    //服务不可用
                    val nsi = si.copy(status = Some(Unavailable))
                    //update nsi
                    MemCache.selectActorFromContext ! PutMsg(nsi.uid.get, nsi)
                    PersistedStore.selectActorFromContext ! UpdateServiceInstanceMsg(nsi)
                }
              }

              opt.onFailure {
                case t =>
                  logger.error("check heartbeat error", t)
                  //服务lost
                  val nsi = si.copy(status = Some(Lost))
                  MemCache.selectActorFromContext ! PutMsg(nsi.uid.get, nsi)
                  PersistedStore.selectActorFromContext ! UpdateServiceInstanceMsg(nsi)
              }

          }
        }
        val _self = self
        Future {
          Thread.sleep(duration.toMillis)
          _self ! CheckAllMsg(duration)
        }
    }

    def checkHeartbeat[A](si: ServiceInstance)(f: HttpResponse => A)(implicit ec: ExecutionContext): Option[Future[A]] = {
      val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
      val ck = for {
        b <- si.baseUrl
        s <- si.service
        ep <- s.healthCheck
      } yield (ep.method, b + ep.url)

      ck.map {
        case (m, u) => m match {
          case GET =>
            pipeline(Get(u))
          case _ =>
            pipeline(Head(u))
        }
      }.map {
        case future => future.map(f)
      }
    }
  }

  object Heartbeat {

    case class CheckAllMsg(duration: Duration)

    def createActor(implicit system: ActorSystem): ActorRef = {
      system.actorOf(Props[Heartbeat], "heartbeat")
    }

    def selectActorFromContext(implicit context: ActorContext): ActorSelection = {
      context.actorSelection("/user/heartbeat")
    }
  }

  class ServiceInstanceFilterActor extends BaseActor {
    import context.dispatcher
    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case QueryServiceInstanceMsg(client, entity) =>
        val bodyString = entity.asString
        import Protocol.ServiceProtocol._
        val serviceInstanceFilter = bodyString.parseJson.convertTo[ServiceInstanceFilter]
        implicit val storeTimeout = Timeout(60.seconds)
        val future = PersistedStore.selectActorFromContext ? FilterServiceInstanceMsg(serviceInstanceFilter)
        future.onSuccess {
          case x: Vector[_/*ServiceInstance*/] =>
            import spray.http.ContentTypes._
            val response = HttpResponse(entity = HttpEntity(contentType = `application/json`,
              x.asInstanceOf[Vector[ServiceInstance]].toJson.compactPrint))
            client ! response
        }
    }
  }

  object ServiceInstanceFilterActor {
    case class QueryServiceInstanceMsg(sender: ActorRef, entity: HttpEntity)

    def createChildActor(implicit context: ActorContext): ActorRef = {
      context.actorOf(Props[ServiceInstanceFilterActor], "service-instance-filter")
    }

    def selectActorFromContext(implicit context: ActorContext): ActorSelection = {
      context.actorSelection("/user/http-listener/service-instance-filter")
    }
  }
}
