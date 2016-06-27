package com.barcsys.http.registry

import com.barcsys.http.registry.Types.Method.HEAD
import com.barcsys.http.registry.Types.ServiceInstanceStatus.Waiting
import com.barcsys.http.registry.Types.ServiceStatus.Enabled

/**
  * Types
  * Created by bigknife on 2016/6/21.
  */
object Types {

  object Implicits {
    implicit def serviceToVector(obj: Service): Vector[(String, Option[Any])] = Vector(
      "uid" -> obj.uid,
      "id" -> obj.id,
      "name" -> obj.name,
      "healthCheck" -> obj.healthCheck,
      "owner" -> obj.owner,
      "org" -> obj.org,
      "source" -> obj.source,
      "version" -> obj.version,
      "endpoints" -> obj.endpoints,
      "tags" -> obj.tags,
      "status" -> obj.status
    )

    implicit def serviceInstanceToVector(obj: ServiceInstance): Vector[(String, Option[Any])] =
      Vector(
      "uid" -> obj.uid,
      "pid" -> obj.pid,
      "host" -> obj.host,
      "port" -> obj.port,
      "baseUrl" -> obj.baseUrl,
      "service" -> obj.service,
      "upTime" -> obj.upTime,
      "tags" -> obj.tags,
      "status" -> obj.status
    )
  }

  /**
    * Service status, Enabled or Disabled
    */
  sealed trait ServiceStatus

  object ServiceStatus {

    case object Enabled extends ServiceStatus

    case object Disabled extends ServiceStatus

    def apply(status: String): ServiceStatus = status match {
      case "Enabled" => Enabled
      case _ => Disabled
    }
  }

  /**
    * Service Instance Status
    *
    * Service instance status, Live, Dead, Waiting or Timeout.
    * Waiting -(healthCheck 200)-> Live -(healthCheck !200) -> Dead
    * if healthCheck timeout, status transition of Timeout
    * when the health check not confirmed, it's UnKnown.
    */
  sealed trait ServiceInstanceStatus

  object ServiceInstanceStatus {

    case object Running extends ServiceInstanceStatus

    case object Lost extends ServiceInstanceStatus

    case object Waiting extends ServiceInstanceStatus

    case object Unavailable extends ServiceInstanceStatus

    def apply(status: String): ServiceInstanceStatus = status match {
      case "Running" => Running
      case "Lost" => Lost
      case "Unavailable" => Unavailable
      case "Waiting" => Waiting
    }
  }


  sealed trait Method


  object Method {

    case object GET extends Method

    case object HEAD extends Method

    case object PUT extends Method

    case object POST extends Method

    case class CustomMethod(method: String) extends Method {
      override def toString = method
    }

    case

    object DELETE extends Method

    def apply(method: String): Method = method match {
      case "GET" => GET
      case "HEAD" => HEAD
      case "PUT" => PUT
      case "POST" => POST
      case "DELETE" => DELETE
      case _ => CustomMethod(method)
    }
  }

  case class Tag(key: String, value: Option[String])

  case class Endpoint(method: Method = HEAD, url: String = "/heartbeat", description: String = "")

  /**
    * 服务基本信息
    *
    * @param uid         唯一ID
    * @param id          服务限定名,如: com.weihui.file
    * @param name        服务名称, 如: 文件服务
    * @param healthCheck 健康检查, 如: Head /heartbeat, 默认为 Endpoint(Head, /heartbeat)
    * @param owner       服务属主
    * @param org         服务所属组织
    * @param source      服务源码地址
    * @param version     服务版本号
    * @param endpoints   服务端点列表
    * @param tags        服务的附加属性
    * @param status      服务状态
    */
  case class Service(
                      uid: Option[String],
                      id: Option[String],
                      name: Option[String],
                      healthCheck: Option[Endpoint] = Some(Endpoint()),
                      owner: Option[String],
                      org: Option[String],
                      source: Option[String],
                      version: Option[String],
                      endpoints: Option[Vector[Endpoint]],
                      tags: Option[Vector[Tag]],
                      status: Option[ServiceStatus] = Some(Enabled)
                    )

  /**
    * 服务实例信息
    *
    * @param uid     实例唯一ID
    * @param pid     实例运行的进程ID
    * @param host    服务主机信息, ip or hostName
    * @param port    服务监听的端口
    * @param baseUrl 服务访问的基URL, 直接访问到该服务的地址,一般就是 http://host:port/, 但在某些环境下需要自定义baseUrl(如容器环境)
    * @param service 所属服务对象, NotNull
    * @param upTime  服务启动时间(自1970/1/1开始的毫秒数)
    * @param tags    服务实例的附加属
    * @param status  实例状态
    *
    */
  case class ServiceInstance(
                              uid: Option[String],
                              pid: Option[Int],
                              host: Option[String],
                              port: Option[Int],
                              baseUrl: Option[String],
                              service: Option[Service],
                              upTime: Option[Long],
                              tags: Option[Vector[Tag]],
                              status: Option[ServiceInstanceStatus] = Some(Waiting)
                            )

}
