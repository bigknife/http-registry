import com.earldouglas.xwp.ContainerPlugin._
import sbt.Credentials
import sbt.Keys._

lazy val commonSettings = Seq(
  organization := "com.barcsys",
  version := "0.1.0.0-SNAPSHOT",
  scalaVersion := "2.11.8",
  resolvers += "spray repo" at "http://repo.spray.io",

  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
  libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.6",
  libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.4.6",
  libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided",
  libraryDependencies += "com.typesafe" % "config" % "1.3.0",
  libraryDependencies += "io.spray" %% "spray-json" % "1.3.2",
  libraryDependencies += "io.spray" %% "spray-client" % "1.3.2",
  libraryDependencies += "io.dropwizard.metrics" % "metrics-core" % "3.1.2",
  libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3"
)


lazy val http_registry = (project in file("."))
  .settings(commonSettings: _*)
  .enablePlugins(WarPlugin)
  .enablePlugins(TomcatPlugin)
  .settings(
    libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "1.1.1",
    webappPostProcess := {
      webappDir =>
        def listFiles(level: Int)(f: File): Unit = {
          val indent = ((1 until level) map { _ => "  " }).mkString
          if (f.isDirectory) {
            streams.value.log.info(indent + f.getName + "/")
            f.listFiles foreach {
              listFiles(level + 1)
            }
          } else streams.value.log.info(indent + f.getName)
        }
        listFiles(1)(webappDir)
    },
    javaOptions in Tomcat += "-DhttpRegistryMongo=mongo://10.65.178.34:27017/http_registry",
    javaOptions in Tomcat += "-DhttpRegistryHost=localhost",
    javaOptions in Tomcat += "-DhttpRegistryPort=9200",

    commands += debugTomcat
  )

def debugTomcat = Command.command("debugTomcat") { state =>
  import com.earldouglas.xwp.ContainerPlugin.start
  val javaOpts =
    Seq(
      "-DwaitWhenInit=5",
      "-Xdebug",
      "-Xrunjdwp:server=y,transport=dt_socket,address=5005,suspend=n"
    )
  val state2 =
    Project.extract(state).append(
      Seq(javaOptions in Tomcat ++= javaOpts),
      state
    )
  Project.extract(state2).runTask(start in Tomcat, state2)
  state2
}