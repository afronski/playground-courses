package kvstore

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

import akka.actor.ActorSystem

import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.testkit.ImplicitSender

import Arbiter._
import Replicator._

class Step6_NewSecondarySpec extends TestKit(ActorSystem("Step6NewSecondarySpec"))
                              with FunSuite
                              with BeforeAndAfterAll
                              with ShouldMatchers
                              with ImplicitSender
                              with Tools {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  test("Case 1: Primary must start replication to new replicas") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case1-primary")
    val user = session(primary)
    val secondary = TestProbe()

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    user.setAcked("k1", "v1")
    arbiter.send(primary, Replicas(Set(primary, secondary.ref)))

    secondary.expectMsg(Snapshot("k1", Some("v1"), 0L))
    secondary.reply(SnapshotAck("k1", 0L))

    val ack1 = user.set("k1", "v2")

    secondary.expectMsg(Snapshot("k1", Some("v2"), 1L))
    secondary.reply(SnapshotAck("k1", 1L))
    user.waitAck(ack1)

    val ack2 = user.remove("k1")

    secondary.expectMsg(Snapshot("k1", None, 2L))
    secondary.reply(SnapshotAck("k1", 2L))

    user.waitAck(ack2)
  }

  test("Case 2: Primary must stop replication to removed replicas and stop Replicator") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case2-primary")
    val user = session(primary)
    val secondary = TestProbe()

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)
    arbiter.send(primary, Replicas(Set(primary, secondary.ref)))

    val ack1 = user.set("k1", "v1")

    secondary.expectMsg(Snapshot("k1", Some("v1"), 0L))

    val replicator = secondary.lastSender

    secondary.reply(SnapshotAck("k1", 0L))
    user.waitAck(ack1)

    watch(replicator)
    arbiter.send(primary, Replicas(Set(primary)))
    expectTerminated(replicator)
  }

  test("Case 3: Primary must stop replication to removed replicas and waive their outstanding acknowledgements") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case3-primary")
    val user = session(primary)
    val secondary = TestProbe()

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)
    arbiter.send(primary, Replicas(Set(primary, secondary.ref)))

    val ack1 = user.set("k1", "v1")

    secondary.expectMsg(Snapshot("k1", Some("v1"), 0L))
    secondary.reply(SnapshotAck("k1", 0L))

    user.waitAck(ack1)

    val ack2 = user.set("k1", "v2")

    secondary.expectMsg(Snapshot("k1", Some("v2"), 1L))
    arbiter.send(primary, Replicas(Set(primary)))

    user.waitAck(ack2)
  }
}