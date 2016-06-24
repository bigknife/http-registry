package com.barcsys.http.registry

import java.util.function.Consumer

import com.barcsys.http.registry.Misc.ConnectionString
import com.barcsys.http.registry.Types._
import com.barcsys.http.registry.Types.ServiceStatus.Disabled
import com.mongodb.ServerAddress
import org.bson.BsonValue
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.{Document, MongoClient, MongoClientSettings, MongoCredential}
import org.mongodb.scala.connection.ClusterSettings

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

/**
  * Store. persist types
  * Created by bigknife on 16/6/22.
  */
object Store {

  trait TypeStore {
    /**
      * 保存服务发现参数
      *
      * @param discovery 服务发现参数
      * @return 服务实例ID列表
      */
    def saveService(discovery: ServiceInstanceDiscovery): Either[Throwable, Vector[String]]

    /**
      * 查询服务发现对象
      *
      * @param id 存储时返回的id
      * @return
      */
    def getService(id: String): Either[Throwable, Option[ServiceInstanceDiscovery]]
  }

  class MongoTypeStore(conn: ConnectionString) extends TypeStore {

    lazy val dbName = conn.path.getOrElse("http_registry").dropWhile(_ == '/')
    lazy val mongoClient = {
      val settingsBuilder = MongoClientSettings.builder()

      import scala.collection.JavaConverters._
      val hostList = conn.addresses.fold(Vector.empty[ServerAddress]) {
        case addresses => addresses.map {
          case (x, oi) => new ServerAddress(x, oi.getOrElse(27017))
        }
      }

      settingsBuilder.clusterSettings(ClusterSettings.builder().hosts(hostList.toList.asJava).build())

      for {
        user <- conn.user
        password <- conn.password
      } yield {
        settingsBuilder.credentialList(List(MongoCredential.createPlainCredential(dbName, user, password.toCharArray)).asJava)
      }

      MongoClient(settingsBuilder.build())
    }

    def saveService(discovery: ServiceInstanceDiscovery): Either[Throwable, Vector[String]] = {

      Try {
        val ret = documentOfDiscovery(discovery)
        val docs = ret.map(_._2)
        val ids = ret.map(_._1)

        ret.foreach {
          case (id, doc) =>
            //query if id has existed
            val obs = mongoClient.getDatabase(dbName).getCollection("service_instances").count(Document("_id" -> id))
            Await.result(obs.toFuture(), 60.seconds) match {
              case Seq(0) =>
                Await.result(
                  mongoClient.getDatabase(dbName).getCollection("service_instances").insertOne(doc).toFuture(),
                  60.seconds
                )

              case Seq(1) => //has existed , replaced it
                Await.result(
                  mongoClient.getDatabase(dbName).getCollection("service_instances").replaceOne(Document("_id" -> id), doc).toFuture(),
                  60.seconds
                )

              case _ => throw new Exception("MongoDB Insert Error")
            }
        }
        ids
      } match {
        case Success(ids) => Right(ids)
        case Failure(t) => Left(t)
      }
    }

    def bsonArrayToVector[A](bsonArray: BsonArray)(f: BsonValue => A): Vector[A] = {
      import scala.collection.JavaConverters._
      bsonArray.asScala.foldLeft(Vector.empty[BsonValue])(_ :+ _).map(f)
    }

    def getService(id: String): Either[Throwable, Option[ServiceInstanceDiscovery]] = {
      Try {
        val obs = mongoClient.getDatabase(dbName).getCollection("service_instances").find(Document("_id" -> id))
        Await.result(obs.toFuture(), 10.seconds) match {
          case Seq(d) =>
            val _id = d.get("_id").map(_.asString().getValue)
            val serviceInstance = {
              val _pid: Option[Int] = d.get("pid").map(_.asInt32.intValue)
              val _host: Option[String] = d.get("host").map(_.asString().getValue)
              val _port: Option[Int] = d.get("port").map(_.asInt32.intValue)
              val _baseUrl: Option[String] = d.get("baseUrl").map(_.asString.getValue)
              val _serviceId: Option[String] = d.get("serviceId").map(_.asString.getValue)
              val _serviceVersion: Option[String] = d.get("serviceVersion").map(_.asString.getValue)
              val _upTime: Option[Long] = d.get("upTime").map(_.asInt64.longValue)
              val _tags: Option[Vector[Tag]] = d.get("tags").asInstanceOf[Option[BsonArray]].map {
                case bsonArray =>
                  bsonArrayToVector(bsonArray) {
                    case bsonValue =>
                      val tagd = bsonValue.asDocument()
                      Tag(key = tagd.getString("key").getValue, value = tagd.getString("value").getValue)
                  }
              }
              ServiceInstance(_pid, _host, _port, _baseUrl, _serviceId, _serviceVersion,
                _upTime, _tags)
            }

            val service = {
              //service document
              val tmp = d.get("service").map(_.asDocument()).get
              val sd: Document = Document(tmp)
              val _id: Option[String] = sd.get("id").map(_.asString.getValue)
              val _version: Option[String] = sd.get("version").map(_.asString.getValue)
              val _name: Option[String] = sd.get("name").map(_.asString.getValue)
              val _owner: Option[String] = sd.get("owner").map(_.asString.getValue)
              val _org: Option[String] = sd.get("org").map(_.asString.getValue)
              val _source: Option[String] = sd.get("source").map(_.asString.getValue)
              val _status: Option[ServiceStatus] = sd.get("name").map(_.asString.getValue).map {
                case s => ServiceStatus(s)
              }

              val _healthCheck: Option[Endpoint] = sd.get("healthCheck").map(bsonValue => Document(bsonValue.asDocument())).map {
                case hcd => Endpoint(Method(hcd.get("method").map(_.asString.toString).get),
                  hcd.get("url").map(_.asString.getValue).get,
                  hcd.get("description").map(_.asString.getValue).get)
              }
              val _tags: Option[Vector[Tag]] = sd.get("tags").asInstanceOf[Option[BsonArray]].map {
                case bsonArray =>
                  bsonArrayToVector(bsonArray) {
                    case bsonValue =>
                      val tagd = bsonValue.asDocument()
                      Tag(key = tagd.getString("key").getValue, value = tagd.getString("value").getValue)
                  }
              }
              val _endpoints: Option[Vector[Endpoint]] = sd.get("endpoints").asInstanceOf[Option[BsonArray]].map {
                case bsonArray =>
                  bsonArrayToVector(bsonArray) {
                    case bsonValue =>
                      val hcd = bsonValue.asDocument()
                      Endpoint(Method(hcd.getString("method").getValue), hcd.getString("url").getValue,
                        hcd.getString("description").getValue)
                  }
              }

              Service(_id, _name, _healthCheck, _owner, _org, _source, _version, _status, _endpoints, _tags)

            }
            Some(ServiceInstanceDiscovery(Vector(serviceInstance), service))

          case _ => None
        }
      } match {
        case Success(x) => Right(x)
        case Failure(t) => Left(t)
      }
    }

    def documentOfDiscovery(discovery: ServiceInstanceDiscovery): Vector[(String, Document)] = {
      discovery.instances.map {
        case inst =>
          val id = discovery.service.id.get + ":" + discovery.service.version.get + ":" + inst.baseUrl.get
          val doc = Document(
            "_id" -> id,
            "pid" -> inst.pid.getOrElse(0),
            "host" -> inst.host.getOrElse("0.0.0.0"),
            "port" -> inst.port.getOrElse(0),
            "baseUrl" -> inst.baseUrl.get, //required
            "serviceId" -> discovery.service.id.get,
            "serviceVersion" -> discovery.service.version.getOrElse("latest"),
            "upTime" -> inst.upTime.getOrElse(System.currentTimeMillis()),
            "tags" -> {
              inst.tags.fold(Vector.empty[Document]) {
                case v => v.map {
                  case t => Document("key" -> t.key, "value" -> t.value)
                }
              }
            },
            "service" -> {
              Document(
                "id" -> discovery.service.id.get,
                "version" -> discovery.service.version.getOrElse("latest"),
                "name" -> discovery.service.name.getOrElse("UnKnown"),
                "healthCheck" -> discovery.service.healthCheck.fold(
                  Document("method" -> "HEAD", "url" -> "/heartbeat", "description" -> "default heartbeat")
                ) {
                  case endpoint => Document("method" -> endpoint.method.toString, "url" -> endpoint.url, "description" -> endpoint.description)
                },
                "owner" -> discovery.service.owner.getOrElse("Nobody"),
                "org" -> discovery.service.org.getOrElse("Free"),
                "source" -> discovery.service.source.getOrElse(""),
                "status" -> discovery.service.status.getOrElse(Disabled).toString,
                "endpoints" -> discovery.service.endpoints.fold(Vector.empty[Document]) {
                  case v => v.map {
                    case endpoint => Document("method" -> endpoint.method.toString, "url" -> endpoint.url, "description" -> endpoint.description)
                  }
                },
                "tags" -> {
                  inst.tags.fold(Vector.empty[Document]) {
                    case v => v.map {
                      case t => Document("key" -> t.key, "value" -> t.value)
                    }
                  }
                }
              )
            }
          )
          (id, doc)
      }
    }

  }

  object MongoTypeStore {
    def apply(conn: ConnectionString): MongoTypeStore = new MongoTypeStore(conn)
  }

}
