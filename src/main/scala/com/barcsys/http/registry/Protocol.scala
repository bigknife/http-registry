package com.barcsys.http.registry

import com.barcsys.http.registry.Types._
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsObject, JsString, JsValue, RootJsonFormat}

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
    def jsObjectValueOf[A](name: String, jsObject: JsObject)(f: JsValue => A): Option[A]  = {
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
  }

  object ServiceInstanceDiscoveryProtocol extends DefaultJsonProtocol {

    implicit object ServiceInstanceDiscoveryFormat extends RootJsonFormat[ServiceInstanceDiscovery] {
      def write(obj: ServiceInstanceDiscovery): JsValue = ???

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
      def write(obj: Service): JsValue = ???

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
      def write(obj: ServiceInstance): JsValue = ???

      def read(json: JsValue): ServiceInstance = ???
    }

    implicit object TagFormat extends RootJsonFormat[Tag] {
      def write(obj: Tag): JsValue = ???

      def read(json: JsValue): Tag = ???
    }

    implicit object EndpointFormat extends RootJsonFormat[Endpoint] {
      def write(obj: Endpoint): JsValue = ???

      def read(json: JsValue): Endpoint = ???
    }

  }

}
