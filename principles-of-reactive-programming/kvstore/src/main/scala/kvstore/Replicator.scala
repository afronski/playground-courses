package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable

import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case class CheckAck(id: Long)
  case object FlushPending

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import context.dispatcher
  import scala.language.postfixOps

  import Replicator._
  import Replica._

  val flushScheduler = context.system.scheduler.schedule(100 milliseconds, 100 milliseconds, self, FlushPending)

  var acknowledgements = Map.empty[Long, (ActorRef, Replicate)]
  var acknowledgementTrackers = Map.empty[Long, Cancellable]
  var pending = Vector.empty[Snapshot]
  var _seqCounter = 0L

  def nextSeq = {
    val ret = _seqCounter

    _seqCounter += 1
    ret
  }

  def receive: Receive = {

    case message@Replicate(key, valueOption, id) => {
      val seq = nextSeq

      acknowledgements += (seq -> (sender, message))
      enqueueForBatchUpdate(Snapshot(key, valueOption, seq))
    }

    case FlushPending => {
      pending.foreach(tellReplicaAndScheduleAckCheck)
      pending = Vector.empty[Snapshot]
    }

    case SnapshotAck(key, seq) => {
      sendReplicated(seq)
    }

    case CheckAck(seq) => {
      acknowledgements.get(seq).map {
        case (_, replicate) => {
          tellReplicaAndScheduleAckCheck(Snapshot(replicate.key, replicate.valueOption, seq))
        }
      }
    }

    case _ => {}
  }

  override def postStop(): Unit = {
    flushScheduler.cancel()
    acknowledgementTrackers.values.foreach(_.cancel())
  }


  private def enqueueForBatchUpdate(snapshot: Snapshot): Unit = {
    val index = pending.indexWhere(snapshot.key == _.key)

    if (index != -1) {
      dequeueExistingSnapshot(index)
    }

    pending :+= snapshot
  }

  private def dequeueExistingSnapshot(index: Int) = {
    sendReplicated(pending(index).seq)
    pending = pending.patch(index, Nil, 1)
  }

  private def tellReplicaAndScheduleAckCheck(snapshot: Snapshot): Unit = {
    replica ! snapshot

    acknowledgementTrackers +=
      (snapshot.seq -> context.system.scheduler.scheduleOnce(150 milliseconds, self, CheckAck(snapshot.seq)))
  }

  private def sendReplicated(seq: Long) = for((op, replicate) <- acknowledgements.get(seq)) {
    removeAckTracker(seq)
    acknowledgements = acknowledgements - seq
    op ! Replicated(replicate.key, replicate.id)
  }

  private def removeAckTracker(seq: Long) = acknowledgementTrackers.get(seq).map { tracker =>
    tracker.cancel()
    acknowledgementTrackers = acknowledgementTrackers - seq
  }
}