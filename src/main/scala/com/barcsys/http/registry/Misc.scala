package com.barcsys.http.registry

import java.io.FileInputStream

import org.slf4j.LoggerFactory
import java.io.InputStream

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator

import scala.util.{Success, Try}

/**
  * Created by bigknife on 16/6/23.
  */
object Misc {

  object LogbackConfigurator {
    val ClasspathProtocol = "classpath://"
    val FileProtocol = "file://"

    /**
      * 重新设定Logback的配置文件
      *
      * @param conf 配置文件定位符,支持"classpath://, file://"
      */
    def resetConfigurations(conf: String*) = {
      conf map {
        case x if x.startsWith(ClasspathProtocol) =>
          streamOf(ClasspathProtocol, x.substring(ClasspathProtocol.length))

        case x if x.startsWith(FileProtocol) =>
          streamOf(FileProtocol, x.substring(FileProtocol.length))

        case _ => None

      } foreach {
        case Some(is) =>
          val context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
          val configurator = new JoranConfigurator
          configurator.setContext(context)
          try {
            context.reset()
            configurator.doConfigure(is)
          } catch {
            case t: Throwable => t.printStackTrace()
          } finally {
            is.close()
          }


        case None => //ignore
      }
    }

    def streamOf(protocol: String, path: String): Option[InputStream] = protocol match {
      case ClasspathProtocol =>
        Try {
          getClass.getResourceAsStream(path)
        } match {
          case Success(is) => Option(is)
          case _ => None
        }

      case FileProtocol =>
        Try {
          new FileInputStream(path)
        } match {
          case Success(is) => Some(is)
          case _ => None
        }
    }
  }

  type Address = (String, Option[Int])

  case class ConnectionString(
                               scheme: String,
                               user: Option[String],
                               password: Option[String],
                               addresses: Option[Vector[Address]],
                               path: Option[String],
                               queries: Option[Map[String, Option[String]]]
                             ) {
    override def toString = {
      scheme + "://" + ((user, password) match {
        case (Some(x), Some(y)) => x + "@" + y + ":"
        case _ => ""
      } ) + (addresses match {
        case None => ""
        case Some(x) => (x map {
          case (host, Some(port)) => host + ":" + port
          case (host, None) => host
        }).mkString(",")
      }) + (path match {
        case None => ""
        case Some(p) => if (p.startsWith("/")) p else "/" + p
      })+ (if (queries.isDefined) "?" else "") + (queries match {
        case None => ""
        case Some(x) => x.foldLeft(Vector.empty[String])({ (vs, kv) =>
          vs :+ (kv match {
            case (k, Some(v)) => k + "=" + v
            case (k, None) => k
          })
        }).mkString("&")
      })
    }
  }

  object ConnectionString {

    //just like `mongo://user:password@10.65.179.34:27017,10.65.179.35:27017/dbName/collectionName?charset=utf-8&user=admin&password=111111`
    def apply(string: String): Option[ConnectionString] = {
      var _scheme: String = null
      var _query: Option[Map[String, Option[String]]] = None
      var _user: Option[String] = None
      var _pass: Option[String] = None
      var _addresses: Option[Vector[Address]] = None
      var _path: Option[String] = None

      string.split("://") match {
        case Array(scheme, withoutScheme) =>
          _scheme = scheme
          withoutScheme.split("[?]") match {
            case Array(userPassAndAddressesAndPath, queries) =>
              _query = Some(parseQueries(queries))
              val (a, b, c, d) = parseUserPasswordAndAddressAndPath(userPassAndAddressesAndPath)
              _user = a
              _pass = b
              _addresses = c
              _path = d

            case Array(userPassAndAddressesAndPath) =>
              val (a, b, c, d) = parseUserPasswordAndAddressAndPath(userPassAndAddressesAndPath)
              _user = a
              _pass = b
              _addresses = c
              _path = d
          }
        case _ =>
      }
      if (_scheme == null) None
      else Some(ConnectionString.apply(_scheme, _user, _pass, _addresses, _path, _query))
    }

    def parseUserPasswordAndAddressAndPath(userPassAndAddressesAndPath: String):
    (Option[String], Option[String], Option[Vector[Address]], Option[String]) = {

      userPassAndAddressesAndPath.split("[/]") match {
        case Array(addresses) =>
          val r = parseUserPasswordAndAddresses(addresses)
          (r._1, r._2, Some(r._3), None)

        case Array(addresses, path) =>
          val r = parseUserPasswordAndAddresses(addresses)
          (r._1, r._2, Some(r._3), Some(path))

        case Array(addresses, path@_*) =>
          val r = parseUserPasswordAndAddresses(addresses)
          (r._1, r._2, Some(r._3), Some(path.mkString("/")))

      }
    }

    def parseUserPasswordAndAddresses(userPasswordAndAddresses: String): (Option[String], Option[String], Vector[Address]) = {
      userPasswordAndAddresses.split("[@]") match {
        case Array(addresses) => (None, None, parseAddresses(addresses))

        case Array(user_password, addresses) =>
          val r = parseUserPassword(user_password)
          (r._1, r._2, parseAddresses(addresses))

        case Array(user_password, addresses@_*) =>
          val r = parseUserPassword(user_password)
          (r._1, r._2, parseAddresses(addresses.mkString("@")))

      }
    }

    def parseUserPassword(userPass: String): (Option[String], Option[String]) = {
      userPass.split("[:]") match {
        case Array(user) => (if (user.length > 0) Some(user) else None, None)

        case Array(user, password) =>
          (if (user.length > 0) Some(user) else None, if (password.length > 0) Some(password) else None)

        case Array(user, password@_*) =>
          val p = password.mkString(":")
          (if (user.length > 0) Some(user) else None, if (p.length > 0) Some(p) else None)

      }
    }

    def parseAddresses(addresses: String): Vector[Address] = {
      val arr = addresses.split("[,]").map {
        case host_port => host_port.split("[:]") match {
          case Array(host, port) => host -> Some(port.toInt)
          case Array(host) => host -> None
        }
      }
      arr.foldLeft(Vector.empty[Address])(_ :+ _)
    }

    def parseQueries(queries: String): Map[String, Option[String]] = {
      val arr = queries.split("[&]").map {
        case k_v =>
          k_v.split("[=]") match {
            case Array(k, v@_*) =>
              val tail = v.mkString("=")
              tail match {
                case "" => k -> None
                case x => k -> Some(x)
              }
          }
      }
      arr.foldLeft(Map.empty[String, Option[String]])(_ + _)
    }
  }

}
