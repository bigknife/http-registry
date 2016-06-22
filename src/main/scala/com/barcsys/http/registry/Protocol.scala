package com.barcsys.http.registry

import com.barcsys.http.registry.Types._
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

import scala.util.{Failure, Success, Try}

/**
  * Created by bigknife on 16/6/22.
  */
object Protocol {

  object JsonUtil {
    def fieldOf(name: String, jsObj: JsObject): Option[JsValue] = {
      jsObj.getFields(name) match {
        case Seq(x) => Some(x)
        case _ => None
      }
    }

    def jsObjectValueOf[A](name: String, jsObject: JsObject)(f: JsValue => A): Option[A] = {
      fieldOf(name, jsObject) match {
        case Some(jsv) => Option(f(jsv))
        case None => None
      }
    }

    def jsArrayValueOf[A](name: String, jsObject: JsObject)(f: JsValue => A): Option[Vector[A]] = {
      fieldOf(name, jsObject) match {
        case Some(JsArray(vector)) =>
          Option(vector.map {
            case jsv => f(jsv)
          })

        case None => None
      }
    }

    def stringOf(name: String, jsObj: JsObject): Option[String] = {
      fieldOf(name, jsObj) match {
        case Some(JsString(x)) => Some(x)
        case _ => None
      }
    }

    def bigdecimalOf(name: String, jsObject: JsObject): Option[BigDecimal] = {
      fieldOf(name, jsObject) match {
        case Some(JsNumber(x)) => Some(x)
        case _ => None
      }
    }

    def intOf(name: String, jsObject: JsObject): Option[Int] = {
      bigdecimalOf(name, jsObject).map(_.intValue())
    }

    def longOf(name: String, jsObject: JsObject): Option[Long] = {
      bigdecimalOf(name, jsObject).map(_.longValue())
    }
  }

  object ServiceInstanceDiscoveryProtocol extends DefaultJsonProtocol {

    implicit object ServiceInstanceDiscoveryFormat extends RootJsonFormat[ServiceInstanceDiscovery] {
      def write(obj: ServiceInstanceDiscovery): JsValue = {
        JsObject("service" -> ServiceFormat.write(obj.service),
          "instances" -> JsArray(obj.instances.map(ServiceInstanceFormat.write)))
      }

      def read(json: JsValue): ServiceInstanceDiscovery = {
        val jsObj = json.asJsObject

        //parse service and instances
        jsObj.getFields("service", "instances") match {
          case Seq(srv, ins) =>
            val service = ServiceFormat.read(srv)

            val instanceArr = ins.asInstanceOf[JsArray]
            val serviceInstances = instanceArr.elements.map {
              case jsValue => ServiceInstanceFormat.read(jsValue)
            }

            ServiceInstanceDiscovery(serviceInstances, service)

          case _ => throw new DeserializationException("ServiceInstanceDiscovery expected")
        }

      }
    }

    implicit object ServiceFormat extends RootJsonFormat[Service] {
      def write(obj: Service): JsValue = {
        var map: Map[String, JsValue] = Map.empty

        map = map ++ obj.id.map("id" -> JsString(_))
        map = map ++ obj.name.map("name" -> JsString(_))
        map = map ++ obj.owner.map("owner" -> JsString(_))
        map = map ++ obj.org.map("org" -> JsString(_))
        map = map ++ obj.source.map("source" -> JsString(_))
        map = map ++ obj.version.map("version" -> JsString(_))
        map = map ++ obj.status.map(status => "status" -> JsString(status.toString))

        map = map ++ obj.healthCheck.map("healthCheck" -> EndpointFormat.write(_))
        map = map ++ obj.endpoints.map("endpoints" -> _.map(EndpointFormat.write))
        map = map ++ obj.tags.map("tags" -> _.map(TagFormat.write))


        JsObject(fields = map)
      }

      def read(json: JsValue): Service = {
        import JsonUtil._
        val jso = json.asJsObject
        val _id = stringOf("id", jso)
        val _name = stringOf("name", jso)
        val _org = stringOf("org", jso)
        val _owner = stringOf("owner", jso)
        val _source = stringOf("source", jso)
        val _version = stringOf("version", jso)
        val _status = stringOf("status", jso).map(ServiceStatus.apply)

        val _healthCheck = jsObjectValueOf("healthCheck", jso)(EndpointFormat.read)
        val _endpoints = jsArrayValueOf("endpoints", jso)(EndpointFormat.read)
        val _tags = jsArrayValueOf("tags", jso)(TagFormat.read)

        Service(_id, _name, _healthCheck, _owner, _org, _source, _version, _status, _endpoints, _tags)
      }
    }

    implicit object ServiceInstanceFormat extends RootJsonFormat[ServiceInstance] {
      def write(obj: ServiceInstance): JsValue = {
        var map: Map[String, JsValue] = Map.empty

        map = map ++ obj.pid.map("pid" -> JsNumber(_))
        map = map ++ obj.host.map("host" -> JsString(_))
        map = map ++ obj.port.map("port" -> JsNumber(_))
        map = map ++ obj.baseUrl.map("baseUrl" -> JsString(_))
        map = map ++ obj.serviceId.map("serviceId" -> JsString(_))
        map = map ++ obj.serviceVersion.map("serviceVersion" -> JsString(_))
        map = map ++ obj.upTime.map("upTime" -> JsNumber(_))
        map = map ++ obj.tags.map("tags" -> _.map(TagFormat.write))


        JsObject(fields = map)
      }

      def read(json: JsValue): ServiceInstance = {
        val jso = json.asJsObject
        import JsonUtil._
        val _pid = intOf("pid", jso)
        val _host = stringOf("host", jso)
        val _port = intOf("port", jso)
        val _baseUrl = stringOf("baseUrl", jso)
        val _serviceId = stringOf("serviceId", jso)
        val _serviceVersion = stringOf("serviceVersion", jso)
        val _upTime = longOf("upTime", jso)
        val _tags = jsArrayValueOf("tags", jso)(TagFormat.read)

        ServiceInstance(_pid, _host, _port, _baseUrl, _serviceId, _serviceVersion, _upTime, _tags)
      }
    }

    implicit object TagFormat extends RootJsonFormat[Tag] {
      def write(obj: Tag): JsValue = JsObject(
        "key" -> JsString(obj.key),
        "value" -> JsString(obj.value)
      )

      def read(json: JsValue): Tag = {
        json.asJsObject.getFields("key", "value") match {
          case Seq(JsString(k), JsString(v)) => Tag(k, v)

          case _ => throw new DeserializationException("Tag expected")
        }
      }
    }

    implicit object EndpointFormat extends RootJsonFormat[Endpoint] {
      def write(obj: Endpoint): JsValue = JsObject(
        "method" -> JsString(obj.method.toString),
        "url" -> JsString(obj.url),
        "description" -> JsString(obj.description)
      )


      def read(json: JsValue): Endpoint = {
        json.asJsObject.getFields("method", "url", "description") match {
          case Seq(JsString(m), JsString(u), JsString(d)) => Endpoint(Method(m), u, d)
          case _ => throw new DeserializationException("Endpoint expected")
        }
      }
    }

  }

}
