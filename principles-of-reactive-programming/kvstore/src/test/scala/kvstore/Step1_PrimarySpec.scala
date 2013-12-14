package kvstore

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers

import akka.actor.ActorSystem

import akka.testkit.TestKit
import akka.testkit.ImplicitSender
import akka.testkit.TestProbe

import scala.concurrent.duration._
import scala.util.Random
import scala.util.control.NonFatal

import kvstore.Persistence.{ Persisted, Persist }
import kvstore.Replica.OperationFailed
import kvstore.Replicator.{ Snapshot }

class Step1_PrimarySpec extends TestKit(ActorSystem("Step1PrimarySpec"))
                          with FunSuite
                          with BeforeAndAfterAll
                          with ShouldMatchers
                          with ImplicitSender
                          with Tools {

  import Arbiter._

  override def afterAll(): Unit = {
    system.shutdown()
  }

  test("Case 1: Primary (in isolation) should properly register itself to the provided Arbiter") {
    val arbiter = TestProbe()

    system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case1-primary")

    arbiter.expectMsg(Join)
  }

  test("Case 2: Primary (in isolation) should react properly to Insert, Remove, Get") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case2-primary")
    val client = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    client.getAndVerify("k1")
    client.setAcked("k1", "v1")
    client.getAndVerify("k1")
    client.getAndVerify("k2")
    client.setAcked("k2", "v2")
    client.getAndVerify("k2")
    client.removeAcked("k1")
    client.getAndVerify("k1")
  }
}