package linksCollector

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

object Receptionist {
  case class Get(url: String)
  case class Failed(url: String)
  case class Result(url: String, links: Set[String])
}

class Receptionist extends Actor {
  import Receptionist._

  def receive = waiting

  case class Job(client: ActorRef, url: String)

  var requestNumber = 0

  val waiting: Receive = {
    case Get(url) => {
      context.become(runNext(Vector(Job(sender, url))))
    }
  }

  def running(queue: Vector[Job]): Receive = {
    case Controller.Result(links) => {
      val job = queue.head

      job.client ! Result(job.url, links)

      context.stop(sender)
      context.become(runNext(queue.tail))
    }

    case Get(url) => {
      context.become(enqueueJob(queue, Job(sender, url)))
    }
  }

  def runNext(queue: Vector[Job]): Receive = {
    requestNumber += 1
    if (queue.isEmpty) {
      waiting
    } else {
      val controller = context.actorOf(Props[Controller], s"count$requestNumber")

      controller ! Controller.Check(queue.head.url, 2)
      running(queue)
    }
  }

  def enqueueJob(queue: Vector[Job], job: Job): Receive = {
    if (queue.size > 3) {
      sender ! Failed(job.url)

      running(queue)
    } else {
      running(queue :+ job)
    }
  }
}