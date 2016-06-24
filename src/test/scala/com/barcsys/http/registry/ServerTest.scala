package com.barcsys.http.registry

/**
  * Created by bigknife on 16/6/23.
  */
object ServerTest extends App{
  import Misc.LogbackConfigurator._
  resetConfigurations("classpath:///META-INF/logback/logback-test.xml")
  Server.start("localhost", 9200)

}
