package com.barcsys.http.registry

import javax.servlet.ServletConfig
import javax.servlet.http.HttpServlet

import Misc.LogbackConfigurator._
import org.slf4j.LoggerFactory

import scala.concurrent.Future

/**
  * Created by bigknife on 16/6/28.
  */
object Startup {

  class StartupServlet extends HttpServlet {
    resetConfigurations("classpath:///META-INF/logback/logback-test.xml",
    "file:///opt/pay/config/basis/inf/http-registry/logback.xml")

    val logger = LoggerFactory.getLogger(getClass)

    override def init(servletConfig: ServletConfig): Unit = {
      super.init(servletConfig)

      logger.warn(
        """
          | interface listened use system env: HTTP_REGISTRY_HOST, jvm env: -DhttpRegistryHost
          | port listened use system env: HTTP_REGISTRY_PORT, jvm env: -DhttpRegistryPort
          | mongo connection string use system env: HTTP_REGISTRY_MONGO, jvm env: -DhttpRegistryMongo
        """.stripMargin)

      val interface = Option(System.getenv("HTTP_REGISTRY_HOST")).getOrElse(
        Option(System.getProperty("httpRegistryHost")).getOrElse("localhost")
      )
      val port = Option(System.getenv("HTTP_REGISTRY_PORT")).getOrElse(
        Option(System.getProperty("httpRegistryPort")).getOrElse("localhost")
      ).toInt

      Server.start(interface, port)

      logger.warn("server start!")
    }

    override def destroy(): Unit = {
      Server.stop()
      super.destroy()
    }
  }
}
