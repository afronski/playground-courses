package kvstore

import scala.collection.immutable

import akka.actor.{ActorRef, Actor}

object Arbiter {
  case object Join

  case object JoinedPrimary
  case object JoinedSecondary

  case class Replicas(replicas: Set[ActorRef])
}

class Arbiter extends Actor {
  import Arbiter._

  var leader: Option[ActorRef] = None
  var replicas = Set.empty[ActorRef]

  def receive = {
    case Join =>
      if (leader.isEmpty) {
        leader = Some(sender)
        replicas += sender

        sender ! JoinedPrimary
      } else {
        replicas += sender

        sender ! JoinedSecondary
      }

      leader foreach (_ ! Replicas(replicas))
  }
}