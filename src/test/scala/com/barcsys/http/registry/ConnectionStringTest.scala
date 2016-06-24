package com.barcsys.http.registry

import com.barcsys.http.registry.Misc.ConnectionString

/**
  * Created by bigknife on 16/6/23.
  */
object ConnectionStringTest extends App{

  val connectionString = ConnectionString(
    "mongo://http_user:http_pass@10.65.178.34:27017,10.65.178.33:21017/http_registry?replica=2&synchronized")

  println(connectionString.get.toString)

  connectionString match {
    case Some(ConnectionString(_, Some(user), Some(pass), _, _, _)) => println(s"user = $user, and pass = $pass")
    case _ => println("no connection string")
  }

}
