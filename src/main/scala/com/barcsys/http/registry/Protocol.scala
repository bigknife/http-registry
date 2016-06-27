package com.barcsys.http.registry

import com.barcsys.http.registry.Types.Method.HEAD
import com.barcsys.http.registry.Types._
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}


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

    def fieldWith[A](name: String, jsObject: JsObject)(f: JsValue => A): Option[A] = {
      fieldOf(name, jsObject).map(f)
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

        case _ => None
      }
    }

    def stringOf(name: String, jsObj: JsObject): Option[String] = {
      fieldOf(name, jsObj) match {
        case Some(JsString(x)) => Some(x)
        case _ => None
      }
    }

    def stringWith[A](name: String, jsObj: JsObject)(f: String => A): Option[A] = {
      stringOf(name, jsObj).map(f)
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

  object ServiceProtocol extends DefaultJsonProtocol {

    import JsonUtil._

    implicit object TagFormat extends RootJsonFormat[Tag] {
      def read(json: JsValue): Tag = {
        stringOf("key", json.asJsObject) match {
          case None => throw new DeserializationException("Tag expected")
          case Some(k) => Tag(k, stringOf("value", json.asJsObject))
        }
      }

      def write(obj: Tag): JsValue = {
        obj.value match {
          case None => JsObject("key" -> JsString(obj.key))
          case Some(v) => JsObject("key" -> JsString(obj.key), "value" -> JsString(v))
        }
      }
    }

    implicit object EndpointFormat extends RootJsonFormat[Endpoint] {
      def read(json: JsValue): Endpoint = {
        val method = stringOf("method", json.asJsObject)
        val url = stringOf("url", json.asJsObject)
        val description = stringOf("description", json.asJsObject)

        Endpoint(method.fold[Method](HEAD)(Method.apply),
          url.fold[String]("/heartbeat")(x => x),
          description.fold[String]("")(x => x))
      }

      def write(obj: Endpoint): JsValue = JsObject(
        "method" -> JsString(obj.method.toString),
        "url" -> JsString(obj.url),
        "description" -> JsString(obj.description)
      )

    }

    implicit object ServiceFormat extends RootJsonFormat[Service] {
      def read(json: JsValue): Service = {
        val jso = json.asJsObject

        val uid = stringOf("uid", jso)
        val id = stringOf("id", jso)
        val name = stringOf("name", jso)
        val healthCheck = jsObjectValueOf[Endpoint]("healthCheck", jso)(EndpointFormat.read)
        val owner = stringOf("owner", jso)
        val org = stringOf("org", jso)
        val source = stringOf("source", jso)
        val version = stringOf("version", jso)
        val endpoints = jsArrayValueOf[Endpoint]("endpoints", jso)(EndpointFormat.read)
        val tags = jsArrayValueOf[Tag]("tags", jso)(TagFormat.read)
        val status = stringWith[ServiceStatus]("status", jso)(ServiceStatus.apply)

        Service(uid, id, name, healthCheck, owner, org, source, version, endpoints, tags, status)
      }

      def write(obj: Service): JsValue = {
        val map = Vector(
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
        ).foldLeft[Map[String, JsValue]](Map.empty) { (z, p) =>
          p match {
            case (k, Some(x)) => x match {
              case v: String => z + (k -> JsString(v))
              case v: Endpoint => z + (k -> EndpointFormat.write(v))
              case v: Vector[_] if k == "endpoints" =>
                val jsoEndpoints = v.foldLeft[Vector[JsValue]](Vector.empty) { (z0, p0) =>
                  z0 :+ EndpointFormat.write(p0.asInstanceOf)
                }
                z + (k -> JsArray(jsoEndpoints))
              case v: Vector[_] if k == "tags" =>
                val jsoTags = v.foldLeft[Vector[JsValue]](Vector.empty) { (z1, p1) =>
                  z1 :+ TagFormat.write(p1.asInstanceOf)
                }
                z + (k -> JsArray(jsoTags))
              case v: ServiceStatus => z + (k -> JsString(v.toString))
            }
            //if value is None, ignore
            case _ => z
          }
        }

        JsObject(map)
      }
    }

    implicit object ServiceInstanceFormat extends RootJsonFormat[ServiceInstance] {
      def read(json: JsValue): ServiceInstance = {
        val jso = json.asJsObject
        val uid = stringOf("uid", jso)
        val pid = intOf("pid", jso)
        val host = stringOf("host", jso)
        val port = intOf("port", jso)
        val baseUrl = stringOf("baseUrl", jso)
        val service = jsObjectValueOf[Service]("service", jso)(ServiceFormat.read)
        val upTime = longOf("upTime", jso)
        val tags = jsArrayValueOf[Tag]("tags", jso)(TagFormat.read)
        val status = stringWith[ServiceInstanceStatus]("status", jso)(ServiceInstanceStatus.apply)

        ServiceInstance(uid, pid, host, port, baseUrl, service, upTime, tags, status)
      }

      def write(obj: ServiceInstance): JsValue = {
        val map:Map[String, JsValue] = Vector(
          "uid" -> obj.uid,
          "pid" -> obj.pid,
          "host" -> obj.host,
          "port" -> obj.port,
          "baseUrl" -> obj.baseUrl,
          "service" -> obj.service,
          "upTime" -> obj.upTime,
          "tags" -> obj.tags,
          "status" -> obj.status
        ).foldLeft[Map[String,JsValue]](Map.empty){(z, p) =>
          p match {
            case (k, Some(x)) => x match {
              case v: String => z + (k -> JsString(v))
              case v: Int => z + (k -> JsNumber(v))
              case v: Long => z + (k -> JsNumber(v))
              case v: Service => z + (k -> ServiceFormat.write(v))
              case v: ServiceInstanceStatus => z + (k -> JsString(v.toString))
              case v: Vector[_] if k == "tags" =>
                val jsonTags = v.foldLeft[Vector[JsValue]](Vector.empty) {(z0, p0) =>
                  z0 :+ TagFormat.write(p0.asInstanceOf)
                }
                z + (k -> JsArray(jsonTags))
            }

            case _ => z
          }
        }

        JsObject(map)
      }
    }

    implicit object ServiceInstanceFilterFormat extends RootJsonFormat[ServiceInstanceFilter] {
      def read(json: JsValue): ServiceInstanceFilter = {
        val jso = json.asJsObject
        
      }

      def write(obj: ServiceInstanceFilter): JsValue = ???
    }

  }
}
