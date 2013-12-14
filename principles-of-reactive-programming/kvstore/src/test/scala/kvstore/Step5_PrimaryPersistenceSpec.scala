package kvstore

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

import akka.actor.ActorSystem

import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.testkit.ImplicitSender

import scala.concurrent.duration._

import Arbiter._
import Persistence._
import Replicator._

class Step5_PrimaryPersistenceSpec extends TestKit(ActorSystem("Step5PrimaryPersistenceSpec"))
                                    with FunSuite
                                    with BeforeAndAfterAll
                                    with ShouldMatchers
                                    with ImplicitSender
                                    with Tools {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  test("Case 1: Primary does not acknowledge updates which have not been persisted") {
    val arbiter = TestProbe()
    val persistence = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, probeProps(persistence)), "case1-primary")
    val client = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    val setId = client.set("foo", "bar")
    val persistId = persistence.expectMsgPF() {
      case Persist("foo", Some("bar"), id) => id
    }

    client.nothingHappens(100.milliseconds)
    persistence.reply(Persisted("foo", persistId))
    client.waitAck(setId)
  }

  test("Case 2: Primary retries persistence every 100 milliseconds") {
    val arbiter = TestProbe()
    val persistence = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, probeProps(persistence)), "case2-primary")
    val client = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    val setId = client.set("foo", "bar")
    val persistId = persistence.expectMsgPF() {
      case Persist("foo", Some("bar"), id) => id
    }

    persistence.expectMsg(200.milliseconds, Persist("foo", Some("bar"), persistId))
    persistence.expectMsg(200.milliseconds, Persist("foo", Some("bar"), persistId))

    client.nothingHappens(100.milliseconds)
    persistence.reply(Persisted("foo", persistId))
    client.waitAck(setId)
  }

  test("Case 3: Primary generates failure after 1 second if persistence fails") {
    val arbiter = TestProbe()
    val persistence = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, probeProps(persistence)), "case3-primary")
    val client = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    val setId = client.set("foo", "bar")

    persistence.expectMsgType[Persist]
    client.nothingHappens(800.milliseconds)
    client.waitFailed(setId)
  }

  test("Case 4: Primary generates failure after 1 second if global acknowledgement fails") {
    val arbiter = TestProbe()
    val persistence = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case4-primary")
    val secondary = TestProbe()
    val client = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)
    arbiter.send(primary, Replicas(Set(primary, secondary.ref)))

    val setId = client.set("foo", "bar")

    secondary.expectMsgType[Snapshot]
    client.nothingHappens(800.milliseconds)
    client.waitFailed(setId)
  }

  test("Case 5: Primary acknowledges only after persistence and global acknowledgement") {
    val arbiter = TestProbe()
    val persistence = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case5-primary")
    val secondaryA, secondaryB = TestProbe()
    val client = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)
    arbiter.send(primary, Replicas(Set(primary, secondaryA.ref, secondaryB.ref)))

    val setId = client.set("foo", "bar")
    val seqA = secondaryA.expectMsgType[Snapshot].seq
    val seqB = secondaryB.expectMsgType[Snapshot].seq

    client.nothingHappens(300.milliseconds)
    secondaryA.reply(SnapshotAck("foo", seqA))

    client.nothingHappens(300.milliseconds)
    secondaryB.reply(SnapshotAck("foo", seqB))

    client.waitAck(setId)
  }
}