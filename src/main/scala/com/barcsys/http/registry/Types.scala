package com.barcsys.http.registry

/**
  * Types
  * Created by bigknife on 2016/6/21.
  */
object Types {

  /**
    * Service status, Enabled or Disabled
    */
  sealed trait ServiceStatus
  case object Enabled extends ServiceStatus
  case object Disabled extends ServiceStatus
  object ServiceStatus {
    def apply(status: String): ServiceStatus = status match {
      case "Enabled" => Enabled
      case _ => Disabled
    }
  }

  /**
    * Service instance status, Live, Dead, or UnKnown.
    * when the health check not confirmed, it's UnKnown.
    */
  sealed trait ServiceInstanceStatus
  case object Live extends ServiceInstanceStatus
  case object Dead extends ServiceInstanceStatus
  case object UnKnown extends ServiceInstanceStatus
  object ServiceInstanceStatus {
    def apply(status: String): ServiceInstanceStatus = status match  {
      case "Live" => Live
      case "Dead" => Dead
      case _ => UnKnown
    }
  }


  sealed trait Method
  case object GET extends Method
  case object HEAD extends Method
  case object PUT extends Method
  case object POST extends Method
  case object DELETE extends Method
  case class CustomMethod(method: String) extends Method {
    override def toString = method
  }

  object Method {
    def apply(method: String): Method = method match {
      case "GET" => GET
      case "HEAD" => HEAD
      case "PUT" => PUT
      case "POST" => POST
      case "DELETE" => DELETE
      case _ => CustomMethod(method)
    }
  }

  case class Tag(key: String, value: String)

  case class Endpoint(method: Method, url: String, description: String)

  case class Service(
                    id: Option[String],
                    name: Option[String],
                    healthCheck: Option[Endpoint],
                    owner: Option[String],
                    org: Option[String],
                    source: Option[String],
                    version: Option[String],
                    status: Option[ServiceStatus],
                    endpoints: Option[Vector[Endpoint]],
                    tags: Option[Vector[Tag]]
                    )

  case class ServiceInstance(
                            pid: Option[Int],
                            host: Option[String],
                            port: Option[Int],
                            baseUrl: Option[String],
                            serviceId: Option[String],
                            serviceVersion: Option[String],
                            upTime: Option[Long],
                            tags: Option[Vector[Tag]]
                            )

  case class ServiceInstanceDiscovery(
                                     instances: Vector[ServiceInstance],
                                     service: Service
                                     )

}
