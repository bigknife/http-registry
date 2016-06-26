package com.barcsys.http.registry


import com.barcsys.http.registry.Misc.ConnectionString
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

    implicit val endpointToDocument: Endpoint => Document = endpoint => Document(
      "method" -> endpoint.method.toString,
      "url" -> endpoint.url,
      "description" -> endpoint.description)


    implicit val serviceToDocument: Service => Document = s => {
      (Vector.empty[(String, Option[Any])] ++ s)
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

            case _ => z + (k -> v.toString) // "status" and other key 's values can be toString
          }
        }
      }

    }

    implicit val serviceInstance2Document: ServiceInstance => Document = si => {
      val s = Vector.empty[(String, Option[Any])] ++ serviceInstanceToVector(si)
      (Vector.empty[(String, Option[Any])] ++ si)
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
  }


  def saveServiceInstance(serviceInstance: ServiceInstance)
                         (implicit mongoClient: MongoClient, dbName: String,
                          ec: ExecutionContext, f: ServiceInstance => Document): Future[ServiceInstance] = {

    val collectioName = "service_instances"
    val collection = mongoClient.getDatabase(dbName).getCollection(collectioName)
    val updateOptions = UpdateOptions()
    updateOptions.upsert(true)
    collection.updateOne(Document("_id" -> serviceInstance.uid), f(serviceInstance), options = updateOptions)
      .toFuture().map[ServiceInstance](_ => serviceInstance)
  }


}
