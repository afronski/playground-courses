package week5

import akka.actor.Actor
import akka.actor.Props

class CounterMain extends Actor {
  val counter = context.actorOf(Props[Counter], "counter")

  counter ! "increment"
  counter ! "increment"
  counter ! "increment"
  counter ! "increment"
  counter ! "increment"
  counter ! "get"

  def receive = {
    case count: Int => {
      println(s"Count was $count.")
      context.stop(self)
    }
  }
}