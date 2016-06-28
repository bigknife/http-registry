package com.barcsys.http.registry


import com.barcsys.http.registry.Misc.ConnectionString
import com.barcsys.http.registry.Types.Method.GET
import com.barcsys.http.registry.Types.ServiceStatus.Enabled
import com.barcsys.http.registry.Types._
import com.mongodb.ServerAddress
import org.mongodb.scala.{Document, MongoClient, MongoClientSettings, MongoCredential}
import org.mongodb.scala.connection.ClusterSettings
import org.mongodb.scala.model.UpdateOptions

import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Store. persist types
  * Created by bigknife on 16/6/22.
  */
object Store {

  def buildMongoClient(implicit connectionString: ConnectionString, dbName: String): MongoClient = {

    val settingsBuilder = MongoClientSettings.builder()

    import scala.collection.JavaConverters._
    val hostList = connectionString.addresses.fold(Vector.empty[ServerAddress]) {
      case addresses => addresses.map {
        case (x, oi) => new ServerAddress(x, oi.getOrElse(27017))
      }
    }

    settingsBuilder.clusterSettings(ClusterSettings.builder().hosts(hostList.toList.asJava).build())

    for {
      user <- connectionString.user
      password <- connectionString.password
    } yield {
      settingsBuilder.credentialList(List(MongoCredential.createPlainCredential(dbName, user, password.toCharArray))
        .asJava)
    }

    MongoClient(settingsBuilder.build())
  }

  object Implicits {

    import Types.Implicits._

    implicit val tagToDocument: Tag => Document = tag => {
      tag.value match {
        case Some(v) => Document("key" -> tag.key, "value" -> v)
        case None => Document("key" -> tag.key)
      }
    }

    implicit val documentToTag: Document => Tag = doc => {
      val key = doc.get("key").fold[String]("")(_.asString().getValue)
      val value = doc.get("value").fold[Option[String]](None)(x => Some(x.asString().getValue))
      Tag(key, value)
    }

    implicit val endpointToDocument: Endpoint => Document = endpoint => Document(
      "method" -> endpoint.method.toString,
      "url" -> endpoint.url,
      "description" -> endpoint.description)

    implicit val documentToEndpoint: Document => Endpoint = doc => {
      val method = doc.get("method").fold[Method](GET)(x => Method(x.asString().getValue))
      val url = doc.get("url").fold[String]("")(_.asString().getValue)
      val description = doc.get("description").fold("")(_.asString().getValue)
      Endpoint(method, url, description)
    }


    implicit val serviceToDocument: Service => Document = s => {
      (Vector.empty[(String, Option[Any])] ++ serviceToVector(s))
        .foldLeft[Document](Document.empty) { (z, p) =>
        p match {
          case (_, None) => z
          case (k, Some(v)) => k match {
            case "healthCheck" => z + (k -> endpointToDocument(v.asInstanceOf[Endpoint]))
            case "endpoints" =>
              val docs = v.asInstanceOf[Vector[Endpoint]].foldLeft[Vector[Document]](Vector.empty) { (z0, e0) =>
                z0 :+ endpointToDocument(e0)
              }
              z + (k -> docs)

            case "tags" =>
              val docs = v.asInstanceOf[Vector[Tag]].foldLeft[Vector[Document]](Vector.empty) { (z0, e0) =>
                z0 :+ tagToDocument(e0)
              }
              z + (k -> docs)

            case "uid" => z + ("_id" -> v.toString)

            case _ => z + (k -> v.toString) // "status" and other key 's values can be toString
          }
        }
      }
    }

    implicit val documentToService: Document => Service = doc => {
      import scala.collection.JavaConverters._
      val uid = doc.get("_id").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val id = doc.get("id").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val name = doc.get("name").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val owner = doc.get("owner").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val org = doc.get("org").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val source = doc.get("source").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val version = doc.get("version").fold[Option[String]](None)(x => Some(x.asString().getValue))

      val tags = doc.get("tags").fold[Option[Vector[Tag]]](None)(x =>
        Some(x.asArray().getValues.asScala.foldLeft(Vector.empty[Tag]) { (z, bsonvalue) =>
          z :+ documentToTag(Document(bsonvalue.asDocument()))
        }))
      val healthCheck = doc.get("healthCheck").fold[Option[Endpoint]](None)(x =>
        Some(documentToEndpoint(Document(x.asDocument()))))
      val endpoints = doc.get("endpoints").fold[Option[Vector[Endpoint]]](None)(
        x => Some(x.asArray().getValues.asScala.foldLeft(Vector.empty[Endpoint]) { (z, bsonvalue) =>
          z :+ documentToEndpoint(Document(bsonvalue.asDocument()))
        })
      )
      val status = doc.get("status").fold[Option[ServiceStatus]](None)(x =>
        Some(ServiceStatus(x.asString().getValue)))

      Service(uid, id, name, healthCheck, owner, org, source, version, endpoints, tags, status)
    }

    implicit val documentToServiceInstance: Document => ServiceInstance = doc => {
      import scala.collection.JavaConverters._

      val uid = doc.get("_id").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val pid = doc.get("pid").fold[Option[Int]](None)(x => Some(x.asInt32().getValue))
      val host = doc.get("host").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val port = doc.get("port").fold[Option[Int]](None)(x => Some(x.asInt32().getValue))
      val baseUrl = doc.get("baseUrl").fold[Option[String]](None)(x => Some(x.asString().getValue))
      val upTime = doc.get("upTime").fold[Option[Long]](None)(x => Some(x.asInt64().getValue))
      val tags = doc.get("tags").fold[Option[Vector[Tag]]](None)(x =>
        Some(x.asArray().getValues.asScala.foldLeft(Vector.empty[Tag]) { (z, bsonvalue) =>
          z :+ documentToTag(Document(bsonvalue.asDocument()))
        }))
      val status = doc.get("status").fold[Option[ServiceInstanceStatus]](None)(x =>
        Some(ServiceInstanceStatus(x.asString().getValue)))
      val service = doc.get("service").fold[Option[Service]](None)(x =>
        Some(documentToService(Document(x.asDocument()))))
      ServiceInstance(uid, pid, host, port, baseUrl, service, upTime, tags, status)
    }

    implicit val serviceInstance2Document: ServiceInstance => Document = si => {
      (Vector.empty[(String, Option[Any])] ++ serviceInstanceToVector(si))
        .foldLeft[Document](Document.empty) { (z, p) =>
        p match {
          case (_, None) => z
          case (k, Some(v)) => k match {
            case "service" => z + (k -> serviceToDocument(v.asInstanceOf[Service]))
            case "tags" =>
              val docs = v.asInstanceOf[Vector[Tag]].foldLeft[Vector[Document]](Vector.empty) { (z0, e0) =>
                z0 :+ tagToDocument(e0)
              }
              z + (k -> docs)

            case "uid" => z + ("_id" -> v.toString)

            case "status" => z + (k -> v.toString)
            case _ => v match {
              case x: Int => z + (k -> x)
              case x: Long => z + (k -> x)
              case x: String => z + (k -> x)
              case _ => z
            }
          }

        }
      }
    }

    implicit val serviceInstanceFilterToDocument: ServiceInstanceFilter => Document = sif => {

      (Vector.empty[(String, Option[Any])] ++ serviceInstanceFilterToVector(sif))
        .foldLeft[Document](Document.empty) {(z, p) =>
        p match {
          case (_, None) => z
          case (k, Some(v)) => k match {
            case "serviceId" => z + ("service.id" -> v.toString)
            case "serviceVersion" => z + ("service.version" -> v.toString)
            case "serviceTags" =>
              v.asInstanceOf[Vector[Tag]].foldLeft[Document](z) { (z0, e0) =>
                e0.value match {
                  case None => z0 + ("service.tags.key" -> e0.key)
                  case Some(v0) => z0 + ("service.tags.key" -> e0.key, "tags.value" -> v0)
                }
              }
            case "serviceInstanceTags" =>
              v.asInstanceOf[Vector[Tag]].foldLeft[Document](z) { (z0, e0) =>
                e0.value match {
                  case None => z0 + ("tags.key" -> e0.key)
                  case Some(v0) => z0 + ("tags.key" -> e0.key, "tags.value" -> v0)
                }
              }
            case "status" =>
              z + ("status" -> v.toString)

            case _ => z
          }
          case _ => z
        }
      }
    }
  }


  def saveServiceInstance(serviceInstance: ServiceInstance)
                         (implicit mongoClient: MongoClient, dbName: String,
                          ec: ExecutionContext, f: ServiceInstance => Document): Future[ServiceInstance] = {

    lazy val collectioName = "service_instances"
    val collection = mongoClient.getDatabase(dbName).getCollection(collectioName)
    val document = f(serviceInstance)

    val future = collection.insertOne(document).toFuture().map[ServiceInstance](_ => serviceInstance)
    future.onFailure {
      case t => //尝试update一次
        collection.replaceOne(Document("_id" -> serviceInstance.uid.get), document)
    }
    future
  }

  def updateServiceInstance(serviceInstance: ServiceInstance)
                           (implicit mongoClient: MongoClient, dbName: String,
                            ec: ExecutionContext, f: ServiceInstance => Document): Future[ServiceInstance] = {
    lazy val collectioName = "service_instances"
    val collection = mongoClient.getDatabase(dbName).getCollection(collectioName)
    collection.replaceOne(Document("_id" -> serviceInstance.uid.get), f(serviceInstance)).toFuture().map[ServiceInstance](_ => serviceInstance)
  }

  def findEnabledServiceInstances(implicit mongoClient: MongoClient,
                                  dbName: String,
                                  ec: ExecutionContext,
                                  f: Document => ServiceInstance): Future[Vector[ServiceInstance]] = {
    findServiceInstances(None)
  }

  def findServiceInstances(filter: Option[Document])(implicit mongoClient: MongoClient,
                                                     dbName: String,
                                                     ec: ExecutionContext,
                                                     f: Document => ServiceInstance): Future[Vector[ServiceInstance]] = {

    lazy val collectionName = "service_instances"
    val collection = mongoClient.getDatabase(dbName).getCollection(collectionName)

    def mapper(docs: Seq[Document]): Vector[ServiceInstance] =  docs
      .foldLeft[Vector[ServiceInstance]](Vector.empty) { (z, doc) => z :+ f(doc) }

    filter match {
      case None =>
        collection.find().toFuture().map(mapper)
      case Some(fil) =>
        collection.find(fil).toFuture().map(mapper)
    }
  }
}
