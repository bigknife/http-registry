package com.barcsys.http.registry

import akka.actor.{Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props}
import com.barcsys.http.registry.BizActors.HeartbeatCheck.{RetryHeartbeatCheckMsg, SimpleHeartbeatCheckMsg}
import com.barcsys.http.registry.Misc.ConnectionString
import com.barcsys.http.registry.BizActors.ServicesResourceHandler.ServiceOnlineMsg
import com.barcsys.http.registry.CommonActors.MemCache
import com.barcsys.http.registry.Store.MongoTypeStore
import com.barcsys.http.registry.Types.{Method, ServiceInstanceDiscovery}
import spray.can.Http
import spray.http._
import spray.http.StatusCodes._
import spray.http.HttpMethods._
import spray.json._
import akka.pattern.ask
import akka.util.Timeout
import spray.http._
import spray.client.pipelining._

import scala.concurrent.duration._
import com.barcsys.http.registry.CommonActors.MemCache.{GetMsg, PutMsg}

import scala.concurrent.{Await, Future}
import scala.util.Try

/**
  * Created by bigknife on 16/6/23.
  */
object BizActors {

  import BaseActors._

  lazy val mongoStore = {
    val connectString = ConnectionString("mongo://10.65.178.34:27017/http_registry")
    MongoTypeStore(conn = connectString.get)
  }

  class HttpListener extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {

    import com.barcsys.http.registry.BizActors.ServicesResourceHandler._

    lazy val servicesResourceHandler = ServicesResourceHandler.createChildActorRef

    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case _: Http.Connected => sender ! Http.Register(self)

      case HttpRequest(POST, Uri.Path("/v1/services"), _, entity, _) =>
        val client = sender
        servicesResourceHandler ! ServiceOnlineMsg(client, entity)

      case x: HttpRequest => sender ! HttpResponse(status = NotFound, entity = "Resource Not Found")
    }
  }

  class ServicesResourceHandler extends
    Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {

    def wrappedReceive: Receive = {
      case ServiceOnlineMsg(client, entity) =>
        Try {
          import Protocol.ServiceInstanceDiscoveryProtocol._
          //反序列化为对象
          val discovery = entity.asString.parseJson.convertTo[ServiceInstanceDiscovery]
          //todo 调用store存储到mongodb
          mongoStore.saveService(discovery) match {
            case Right(x) =>
              x.foreach {
                case id => //向心跳Actor发送心跳请求
                  HeartbeatCheck.selectActorFromContext ! SimpleHeartbeatCheckMsg(id)
              }
              OK
            case Left(t) =>
              logger.error("save service error!", t)
              InternalServerError
          }
        } match {
          case scala.util.Success(x) => client ! HttpResponse(status = x)

          case scala.util.Failure(t) => t match {
            case x: DeserializationException =>
              logger.error("deserialization error", x)
              client ! HttpResponse(status = BadRequest, entity = x.getMessage)

            case x =>
              logger.error("server internal error", x)
              client ! HttpResponse(status = InternalServerError, entity = x.getMessage)

          }
        }

    }
  }

  object ServicesResourceHandler {

    sealed trait ServicesResourceMsg

    case class ServiceOnlineMsg(sender: ActorRef, entity: HttpEntity) extends ServicesResourceMsg

    def createChildActorRef(implicit context: ActorContext): ActorRef = {
      context.actorOf(Props[ServicesResourceHandler], "services-handler")
    }
  }

  class HeartbeatCheck extends Actor with Slf4jLogging with ActorMetrics with ActorMessageTrack {
    import context.dispatcher
    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive = {
      case SimpleHeartbeatCheckMsg(serviceInstanceId) =>
        findDiscovery(serviceInstanceId) {
          case x =>
            val pipline: HttpRequest => Future[HttpResponse] = sendReceive
            val optReqs = heartbeatRequestOf(x)
            optReqs.foreach {
              case vreq => vreq.foreach {
                case req =>
                  val f = pipline(req)
                  f.onSuccess {
                    case resp =>
                      logger.info("status code = {}", resp.status.intValue)
                      //todo: 心跳间隔由service参数决定,在service里加一个属性heartbeatInterval
                      val _self = self
                      val _serviceInstanceId = serviceInstanceId
                      resp.status match {
                        case OK => {
                          Future {
                            Thread.sleep(3000)
                            _self ! SimpleHeartbeatCheckMsg(_serviceInstanceId)
                          }
                        }
                        case s if s.intValue >= 400 =>
                          //发送重试消息
                          _self ! RetryHeartbeatCheckMsg(_serviceInstanceId, currentTime = 1,
                            maxTimes = 5, delay = 3000)
                      }
                  }
                  f.onFailure {
                    case t =>
                      logger.error("health check error", t)
                      //出错处理, 停止监控,服务下线
                  }
              }
            }
        }
      case RetryHeartbeatCheckMsg(serviceInstanceId, currentTime, maxTimes, delay) =>
        Future {
          Thread.sleep(delay)
          findDiscovery(serviceInstanceId) {
            case x =>
              val pipline: HttpRequest => Future[HttpResponse] = sendReceive
              val optReqs = heartbeatRequestOf(x)
              optReqs.foreach {
                case vreq => vreq.foreach {
                  case req =>
                    val f = pipline(req)
                    f.onSuccess {
                      case resp =>
                        logger.info("status code = {}", resp.status.intValue)
                        //todo: 心跳间隔由service参数决定,在service里加一个属性heartbeatInterval
                        val _self = self
                        val _serviceInstanceId = serviceInstanceId
                        resp.status match {
                          case OK => {
                            Future {
                              Thread.sleep(3000)
                              _self ! SimpleHeartbeatCheckMsg(_serviceInstanceId)
                            }
                          }
                          case s if s.intValue >= 400 =>
                            //发送重试消息
                            _self ! RetryHeartbeatCheckMsg(_serviceInstanceId, currentTime = 1,
                              maxTimes = 5, delay = 3000)
                        }
                    }
                    f.onFailure {
                      case t =>
                        logger.error("health check error", t)
                      //出错处理, 停止监控,服务下线
                    }
                }
              }
          }
        }
    }

    def checkHeartbeat()

    def heartbeatRequestOf(x: ServiceInstanceDiscovery): Option[Vector[HttpRequest]] = {
      x.service.healthCheck.map {
        //仅支持GET和HEAD的heartbeat探针
        case endpoint =>  {
          val serviceHeartbeat = endpoint.method match {
            case Method.GET => (GET, endpoint.url)
            case _ => (HEAD, endpoint.url)
          }

          x.instances.map {
            case y => HttpRequest(serviceHeartbeat._1, Uri(y.baseUrl.get + serviceHeartbeat._2))
          }
        }
      }
    }

    def keyOf(serviceInstanceId: String): String = {
      "discovery:" + serviceInstanceId
    }

    def findDiscovery(serviceInstanceId: String)(thenDo: ServiceInstanceDiscovery => Unit): Unit = {
      //从缓存中查找discovery, 如果没有则到store中查询,然后放到缓存
      implicit val timeout = Timeout(10.seconds)
      val f = Await.result(MemCache.selectActorFromContext ? GetMsg(keyOf(serviceInstanceId)), 10.seconds).asInstanceOf[Option[ServiceInstanceDiscovery]]
      val optDiscovery: Option[ServiceInstanceDiscovery] = f match {
        case None =>
          mongoStore.getService(serviceInstanceId) match {
            case Left(t) =>
              logger.error("find discovery error, serviceInstanceId = {}", Seq(serviceInstanceId, t):_*)
              None

            case Right(Some(x)) =>
              //发送缓存消息
              MemCache.selectActorFromContext ! PutMsg(keyOf(serviceInstanceId), x)
              Some(x)


            case Right(None) =>
              logger.info("discovery not found in store: serviceInstanceId = {}", serviceInstanceId)
              None
          }
        case x => x
      }

      optDiscovery.foreach {
        case x => thenDo(x)
      }
    }
  }

  object HeartbeatCheck {

    sealed trait HeartbeatCheckMsg

    case class SimpleHeartbeatCheckMsg(serviceInstanceId: String) extends HeartbeatCheckMsg

    case class RetryHeartbeatCheckMsg(serviceInstanceId: String, currentTime: Int, maxTimes: Int, delay: Long)

    def createActor(implicit system: ActorSystem): ActorRef = {
      system.actorOf(Props[HeartbeatCheck], "heartbeat-check")
    }

    def selctActorFromSystem(implicit system: ActorSystem): ActorSelection = {
      system.actorSelection("/user/heartbeat-check")
    }

    def selectActorFromContext(implicit context: ActorContext): ActorSelection = {
      context.actorSelection("/user/heartbeat-check")
    }
  }

}
