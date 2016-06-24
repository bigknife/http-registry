package com.barcsys.http.registry

/**
  * Created by bigknife on 16/6/23.
  */
object CompanionObjectTest extends App {

  case class Person(firstName: String, secondName: String)

  object Person {
    def apply(longName: String): Option[Person] = {
      longName.split("[:]") match {
        case Array(x) => Some(new Person(x, null))
        case Array(x, y) => Some(new Person(x, y))
        case _ => None
      }
    }
  }

  val p1 = Person("Zenghui", "Song")

  val p2 = Person("Zenghui:Song").get

  println(p1 == p2)

  p2 match {
    case Person(x, y) => println(x + "....." + y)
  }
}
