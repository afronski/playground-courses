package linksCollector

import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorLogging
import akka.actor.Props
import akka.actor.ReceiveTimeout

object Controller {
  case class Check(url: String, depth: Int) {
    require(depth > -1)
  }

  case class Result(cache: Set[String])
}

class Controller extends Actor with ActorLogging {
  import Controller._

  var cache = Set.empty[String]
  var children = Set.empty[ActorRef]

  context.setReceiveTimeout(10 seconds)

  // Another way for timeouting:
  //
  //  import context.dispatcher
  //
  //  context.system.scheduler.scheduleOnce(10 seconds) {
  //    children foreach (_ ! Getter.Abort)
  //  }

  def receive = {
    case Check(url, depth) => {
      log.debug("{} checking {}", depth, url)

      if (!cache(url) && depth > 0) {
        children += context.actorOf(Props(new Getter(url, depth - 1)))
      }

      cache += url
    }

    case Getter.Done => {
      children -= sender

      if (children.isEmpty) {
        context.parent ! Result(cache)
      }
    }

    case ReceiveTimeout => {
      children foreach (_ ! Getter.Abort)
    }
  }
}