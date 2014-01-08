package kvstore

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.collection.immutable.Queue

import akka.actor.{ OneForOneStrategy, SupervisorStrategy, Props, ActorRef, Actor, Terminated, PoisonPill }
import akka.actor.SupervisorStrategy.Restart
import akka.pattern.{ ask, pipe }
import akka.util.Timeout

import kvstore.Arbiter._

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }

  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case object CheckPersisted
  case class OperationTimeout(id: Long)
  case class OperationRetry(id: Long)

  def props(arbiter: ActorRef, persistenceProps: Props): Props =
    Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import context.dispatcher
  import scala.language.postfixOps

  import Replica._
  import Replicator._
  import Persistence._
  import ConsistencyManager._

  var keyValueStorage = Map.empty[String, String]
  var secondaries = Map.empty[ActorRef, ActorRef]
  var managers = Map.empty[String, ActorRef]

  private var expectedSnapshot = 0

  val persistence = context.actorOf(persistenceProps)

  def receive = {
    case JoinedPrimary => {
      context.become(leader)
    }

    case JoinedSecondary => {
      context.become(replica)
    }
  }

  val leader: Receive = handleGetOperation orElse {
    case Insert(key, value, id) => {
      keyValueStorage = keyValueStorage + (key -> value)
      leaderManager(key) ! Modification(sender, id, key, Some(value))
    }

    case Remove(key, id) => {
      keyValueStorage = keyValueStorage - key
      leaderManager(key) ! Modification(sender, id, key, None)
    }

    case Replicas(replicas) => {
      whenReplicasChange(replicas)
    }

    case _ => {}
  }

  val replica: Receive = handleGetOperation orElse {
    case Snapshot(key, valueOption, id) => {
      if (expectedSnapshot == id) {
        keyValueStorage = valueOption.fold(keyValueStorage - key)(keyValueStorage.updated(key, _))
        expectedSnapshot += 1
      }

      if (id < expectedSnapshot) {
        replicaManager(key) ! Modification(sender, id, key, valueOption)
      }
    }
  }

  override def preStart() {
    arbiter ! Join
  }

  private def handleGetOperation: Receive = {
    case Get(key, id) => {
      sender ! GetResult(key, keyValueStorage.get(key), id)
    }
  }

  private def manager(key: String)(ackBuilder: (Long, String) => Any) =
    managers.get(key).getOrElse {
      val manager = context.actorOf(Props(classOf[ConsistencyManager],
                                          persistence,
                                          secondaries.values.toSet,
                                          ackBuilder))

      managers += (key -> manager)
      manager
    }

  private def leaderManager(key: String) =
    manager(key)((id: Long, key: String) => OperationAck(id))

  private def replicaManager(key: String) =
    manager(key)((seq: Long, key: String) => SnapshotAck(key, seq))

  private def whenReplicasChange(replicas: Set[ActorRef]) =  {
    def isGone(replica: ActorRef) = {
      secondaries.contains(replica) && !replicas.contains(replica)
    }

    def isNew(replica: ActorRef) = {
      !secondaries.contains(replica) && replicas.contains(replica)
    }

    def handleReplicaRemoval(replica: ActorRef) = {
      managers.values.foreach(_ ! Gone(secondaries(replica)))
      context.stop(secondaries(replica))
      secondaries -= replica
    }

    def handleReplicaCreation(replica: ActorRef) = {
      val replicator = context.actorOf(Props(classOf[Replicator], replica))

      managers.values.foreach(_ ! Entered(replicator))
      secondaries += (replica -> replicator)

      keyValueStorage.foldLeft(0) {
        case (seq, (key, value)) => {
          replicator ! Replicate(key, Some(value), seq)
          seq - 1
        }
      }
    }

    for (replica <- (replicas - self) ++ secondaries.keys.toSet) {
      if (isGone(replica)) {
        handleReplicaRemoval(replica)
      } else if (isNew(replica)) {
        handleReplicaCreation(replica)
      }
    }
  }
}