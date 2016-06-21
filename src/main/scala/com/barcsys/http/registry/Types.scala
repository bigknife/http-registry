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

  /**
    * Service instance status, Live, Dead, or UnKnown.
    * when the health check not confirmed, it's UnKnown.
    */
  sealed trait ServiceInstanceStatus
  case object Live extends ServiceInstanceStatus
  case object Dead extends ServiceInstanceStatus
  case object UnKnown extends ServiceInstanceStatus


  sealed trait Method
  case object GET extends Method
  case object HEAD extends Method
  case object PUT extends Method
  case object POST extends Method
  case object DELETE extends Method

  case class Tag(key: String, value: String)

  case class Endpoint(method: Method, url: String)

  case class Service(
                    id: String,
                    name: String,
                    healthCheck: Endpoint,
                    owner: String,
                    org: String,
                    source: String,
                    version: String,
                    status: ServiceStatus,
                    endpoints: Vector[Endpoint],
                    tags: Vector[Tag]
                    )

  case class ServiceInstance(
                            pid: Int,
                            host: String,
                            port: Int,
                            baseUrl: String,
                            serviceId: String,
                            serviceVersion: String,
                            upTime: Long,
                            tags: Vector[Tag]
                            )

}
