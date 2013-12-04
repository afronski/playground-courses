package linksCollector

import java.util.concurrent.Executor

import scala.concurrent.ExecutionContext

import akka.actor.Actor
import akka.pattern.pipe

object Getter {
  case class Done
  case class Abort
}

class Getter(url: String, depth: Int) extends Actor {
  import Getter._
  import LinksParser._

  implicit val executor = context.dispatcher.asInstanceOf[Executor with ExecutionContext]

  WebClient get url pipeTo self

  def receive = {
    case body: String => {
      for (link <- findLinks(body)) {
        context.parent ! Controller.Check(link, depth)
      }

      stop()
    }

    case _: Status.Failure => {
      stop()
    }

    case Abort => {
      stop()
    }
  }

  def stop(): Unit = {
    context.parent ! Done
    context.stop(self)
  }
}