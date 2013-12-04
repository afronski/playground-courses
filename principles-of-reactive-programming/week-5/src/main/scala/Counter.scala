package week5

import akka.actor.Actor

class Counter extends Actor {
  var count = 0

  def receive = {
    case "increment"  => count += 1
    case "get"        => sender ! count
  }
}