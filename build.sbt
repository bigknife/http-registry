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
  libraryDependencies += "io.spray" %% "spray-client" % "1.3.2"
  )


lazy val http_registry = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
  )
