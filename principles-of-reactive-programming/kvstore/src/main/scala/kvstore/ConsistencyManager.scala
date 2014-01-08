package kvstore

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.duration._

import akka.actor.{ OneForOneStrategy, SupervisorStrategy, Props, ActorRef, Actor, Terminated, PoisonPill }
import akka.actor.SupervisorStrategy.Restart
import akka.pattern.{ ask, pipe }
import akka.util.Timeout

import kvstore.Arbiter._

object ConsistencyManager {
  import Replica._

  case class Modification(requester: ActorRef, id: Long, key: String, valueOption: Option[String])

  sealed trait ReplicatorLifecycle

  case class Gone(replicator: ActorRef) extends ReplicatorLifecycle
  case class Entered(replicator: ActorRef) extends ReplicatorLifecycle

  sealed trait SyncLifecycle {
    def modification: Modification
  }

  case class SyncTimeout(val modification: Modification) extends SyncLifecycle
  case class SyncSuccessful(val modification: Modification) extends SyncLifecycle
  case class EnsurePersisted(val modification: Modification) extends SyncLifecycle
}

class ConsistencyManager(var persistence: ActorRef,
                         var replicators: Set[ActorRef],
                         val buildResponse: (Long, String) => Any) extends Actor {
  import context.dispatcher
  import scala.language.postfixOps

  import akka.actor.Cancellable

  import Replica._
  import Replicator._
  import Persistence._
  import ConsistencyManager._

  var deadlines: Option[Cancellable] = None
  var persitences: Option[Cancellable] = None

  def receive = waiting

  def waiting: Receive = {
    case Gone(replicator) => {
      replicators -= replicator
    }

    case Entered(replicator) => {
      replicators += replicator
    }

    case modification: Modification => {
      context.become(runNext(Queue(modification)))
    }

    case _ => {}
  }

  def runNext(queue: Queue[Modification]): Receive = {
    deadlines.map(_.cancel())
    persitences.map(_.cancel())

    if (queue.isEmpty) {
      waiting
    }
    else {
      val actual = queue.head

      persistence ! Persist(actual.key, actual.valueOption, actual.id)
      replicators.foreach(_ ! Replicate(actual.key, actual.valueOption, actual.id))

      deadlines = Some(context.system.scheduler.scheduleOnce(1 second, self, SyncTimeout(actual)))
      persitences = Some(context.system.scheduler.scheduleOnce(100 milliseconds, self, EnsurePersisted(actual)))

      synchronize(queue, false, replicators)
    }
  }

  def synchronize(queue: Queue[Modification], persisted: Boolean, partners: Set[ActorRef]): Receive = {
    val actual = queue.head

    if (persisted && partners.isEmpty) {
      actual.requester ! buildResponse(actual.id, actual.key)
      runNext(queue.tail)
    } else {
      awaitSynchronization(queue, persisted, partners)
    }
  }

  def awaitSynchronization(queue: Queue[Modification], persisted: Boolean, partners: Set[ActorRef]): Receive = {
    case EnsurePersisted(op) if op == queue.head => {
      persistence ! Persist(op.key, op.valueOption, op.id)
      persitences = Some(context.system.scheduler.scheduleOnce(100 milliseconds, self, EnsurePersisted(op)))
    }

    case Persisted(key, id) if id == queue.head.id => {
      persitences.map(_.cancel())
      context.become(synchronize(queue, persisted = true, partners))
    }

    case Replicated(key, id) if id == queue.head.id => {
      context.become(synchronize(queue, persisted, partners - sender))
    }

    case SyncTimeout(op) if op == queue.head => {
      val actual = queue.head

      actual.requester ! OperationFailed(actual.id)
      context.become(runNext(queue.tail))
    }

    case modification: Modification => {
      context.become(awaitSynchronization(queue :+ modification, persisted, partners))
    }

    case Gone(r) => {
      replicators -= r
      context.become(synchronize(queue, persisted, partners - r))
    }

    case Entered(r) => {
      replicators += r

      val actual = queue.head

      r ! Replicate(actual.key, actual.valueOption, actual.id)
      context.become(synchronize(queue, persisted, partners + r))
    }

    case _ => {}
  }

  override def postStop(): Unit = {
    persitences.map(_.cancel())
    deadlines.map(_.cancel())
  }
}