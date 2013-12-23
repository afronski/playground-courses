package actorbintree

import akka.actor._

import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef =
    context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot
  var pendingQueue = Queue.empty[Operation]

  def receive = normal

  val normal: Receive = {
    case insert: Insert => {
      root ! insert
    }

    case contains: Contains => {
      root ! contains
    }

    case remove: Remove => {
      root ! remove
    }

    case GC => {
      val newRoot = createRoot

      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot

      pendingQueue foreach(root ! _)
      pendingQueue = Queue.empty[Operation]

      context.become(normal)
    }

    case operation: Operation => {
      pendingQueue = pendingQueue.enqueue(operation)
    }

    case GC =>
      // Ignoring another GC messages when one is performed.
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) =
    Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  val SELF_INSERT = -1

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = normal

  def addNewSubTree(where: Position, value: Int) = {
    subtrees updated (where, context.actorOf(BinaryTreeNode.props(value, initiallyRemoved = false)))
  }

  val normal: Receive = {
    case Insert(requester, id, value) => {
      if (value < elem && subtrees.contains(Left)) {
        subtrees.get(Left) match {
          case Some(left) => {
            left ! Insert(requester, id, value)
          }

          case None =>
        }
      }

      if (value > elem && subtrees.contains(Right)) {
        subtrees.get(Right) match {
          case Some(right) => {
            right ! Insert(requester, id, value)
          }

          case None =>
        }
      }

      if (value < elem && !subtrees.contains(Left)) {
        subtrees = addNewSubTree(Left, value)
        requester ! OperationFinished(id)
      }

      if (value > elem && !subtrees.contains(Right)) {
        subtrees = addNewSubTree(Right, value)
        requester ! OperationFinished(id)
      }

      if (value == elem) {
        if (removed) {
          removed = false
        }

        requester ! OperationFinished(id)
      }
    }

    case Contains(requester, id, value) => {
      if (value == elem) {
        requester ! ContainsResult(id, !removed)
      } else {
        if (value < elem) {
          subtrees get(Left) match {
            case Some(left) => {
              left ! Contains(requester, id, value)
            }

            case None => {
              requester ! ContainsResult(id, false)
            }
          }
        }

        if (value > elem) {
          subtrees get(Right) match {
            case Some(right) => {
              right ! Contains(requester, id, value)
            }

            case None => {
              requester ! ContainsResult(id, false)
            }
          }
        }
      }
    }

    case Remove(requester, id, value) => {
      if (value == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else {
        if (value < elem) {
          subtrees get(Left) match {
            case Some(left) => {
              left ! Remove(requester, id, value)
            }

            case None => {
              requester ! OperationFinished(id)
            }
          }
        }

        if (value > elem) {
          subtrees get(Right) match {
            case Some(right) => {
              right ! Remove(requester, id, value)
            }

            case None => {
              requester ! OperationFinished(id)
            }
          }
        }
      }
    }

    case CopyTo(newRoot) => {
      var expected = Set[ActorRef]()

      subtrees get(Left) match {
        case Some(left) => {
          expected = expected + left
        }

        case None =>
      }

      subtrees get(Right) match {
        case Some(right) => {
          expected = expected + right
        }

        case None =>
      }

      if (expected.isEmpty && removed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(expected, removed))
        expected foreach(_ ! CopyTo(newRoot))

        if (!removed) {
          newRoot ! Insert(self, SELF_INSERT, elem)
        }
      }
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished => {
      if (!expected.isEmpty) {
        if ((expected - expected.head).isEmpty) {
          if (insertConfirmed) {
            context.become(normal)
            context.parent ! CopyFinished
          } else {
            context.become(copying(expected - expected.head, insertConfirmed))
          }
        } else {
          context.become(copying(expected - expected.head, insertConfirmed))
        }
      } else {
        if (insertConfirmed) {
          context.become(normal)
          context.parent ! CopyFinished
        }
      }
    }

    case OperationFinished(SELF_INSERT) => {
      context.become(copying(expected, true))

      if (expected.isEmpty) {
        context.become(normal)
        context.parent ! CopyFinished
      }
    }
  }
}