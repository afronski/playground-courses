package nodescala

import scala.language.postfixOps

import scala.util.{Try, Success, Failure}

import scala.collection._
import scala.concurrent._

import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}

import org.scalatest._
import NodeScala._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

case class ExpectedException(message: String) extends Exception

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517, "Exact value has to be returned immediately.")
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false, "This should not be here!")
    } catch {
      case t: TimeoutException =>
    }
  }

  test("With all never() futures, any() can't finish") {
    val any = Future.any(List(Future.never[Int], Future.never[Int]))

    try {
      Await.result(any, 1 second)
      assert(false, "This should not be here!")
    } catch {
      case t: TimeoutException =>
    }
  }

  test("With one proper future and rest of never() futures, any() have to finish") {
    val any = Future.any(List(Future.never[Int], Future { 1 }, Future.never[Int]))

    try {
      assert(Await.result(any, 1 second) == 1, "It should return the fastest result.")
    } catch {
      case t: TimeoutException => assert(false, "This should not timed out!")
    }
  }

  test("When in list there is a never() future, all() can't finish") {
    val all = Future.all(List(Future.never[Int], Future { 1 }, Future.never[Int]))

    try {
      Await.result(all, 1 second)
      assert(false, "This should not be here!")
    } catch {
      case t: TimeoutException =>
    }
  }

  test("When in list there are no never() futures, all() have to finish, with preserved order") {
    val all = Future.all(List(Future { 2 }, Future { 1 }, Future { 3 }))

    try {
      assert(Await.result(all, 1 second) == List(2, 1, 3), "Result has to a list with specified order.")
    } catch {
      case t: TimeoutException => assert(false, "This should not timed out!")
    }
  }

  test("Future.delay have to finish when Await will wait") {
    val delayFuture = Future.delay(1 second)

    try {
      Await.result(delayFuture, 2 seconds)
    } catch {
      case t: TimeoutException => assert(false, "This should not timed out!")
    }
  }

  test("Future.delay can't to finish when Await won't wait") {
    val delayFuture = Future.delay(2 seconds)

    try {
      Await.result(delayFuture, 1 second)
      assert(false, "This should not be here!")
    } catch {
      case t: TimeoutException =>
    }
  }

  test("Future[T].now should return value for definied future") {
    val future = Future.always(12)

    try {
      assert(future.now == 12, "This has to be equal to 12.")
    } catch {
      case t: TimeoutException => assert(false, "This should not timed out!")
      case t: NoSuchElementException => assert(false, "This should not throw!")
    }
  }

  test("Future[T].now should throw for undefinied future") {
    val future = Future.never

    try {
      future.now
      assert(false, "This should not be here!")
    } catch {
      case t: TimeoutException => assert(false, "This should not timed out!")
      case t: NoSuchElementException =>
    }
  }

  test("Future.continueWith() succeeds") {
    val future = Future { 12 }.continueWith((previous: Future[Int]) => {
      Await.result(previous, 1 second) + 12
    })

    assert(Await.result(future, 2 seconds) == 24, "It should sum up both results.")
  }

  test("Future.continueWith() first timing out") {
    val future = Future.never.continueWith((previous: Future[Unit]) => {
      Await.result(previous, 10 seconds)
    })

    try {
      Await.result(future, 2 seconds)
      assert(false, "It should time out!")
    } catch {
      case t: TimeoutException =>
    }
  }

  test("Future.continueWith() first failing") {
    val future = Future {
      throw new ExpectedException("Expected exception")
      1
    }.continueWith((previous: Future[Int]) => {
      previous onComplete {
        case Success(_) => 3
        case Failure(_) => 2
      }

      10 + Await.result(previous, 0 nanos)
    })

    try {
      assert(Await.result(future, 1 second) == 12)
      assert(false, "It should throw!")
    } catch {
      case t: TimeoutException => assert(false, "It should time out!")
      case t: ExpectedException =>
    }
  }

  test("Future.continueWith() second failing") {
    val future = Future { 1 }.continueWith((previous: Future[Int]) => {
      previous onComplete {
        case Success(_) => 3
        case Failure(_) => 2
      }

      throw new ExpectedException("Expected exception")
    })

    try {
      Await.result(future, 1 second)
      assert(false, "It should throw!")
    } catch {
      case t: TimeoutException => assert(false, "It should time out!")
      case t: ExpectedException =>
    }
  }

  test("Test for happy path of continue()") {
    val future = Future.always(10).continue((x: Try[Int]) => {
      x match {
        case Success(v) => v + 2
        case Failure(e) => 0
      }
    })

    try {
      assert(Await.result(future, 1 second) == 12, "Result should be equal to 12.")
    } catch {
      case t: TimeoutException => assert(false, "This should not timeout!")
    }
  }

  test("Test for unhappy path of continue()") {
    val future = Future.never.continue((x: Try[Int]) => {
      x match {
        case Success(v) => v + 2
        case Failure(e) => 0
      }
    })

    try {
      Await.result(future, 1 second)
      assert(false, "This should not be here!")
    } catch {
      case t: TimeoutException =>
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {}

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done", "Result has to be equal to 'done'.")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()

    def write(s: String) {
      response += s
    }

    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "Is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "Is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req, "Next request assertion.")
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)

    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)

      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

  test("Never ending response") {
    val dummy = new DummyServer(8191)

    val dummySubscription = dummy.start("/testDir") {
      _ => Stream.continually(".").iterator
    }

    Thread.sleep(500)

    try {
      val webpage = dummy.emit("/testDir", immutable.Map("Test" -> List()))

      Future.delay(1 second).continue { _ =>
        dummySubscription.unsubscribe()
      }

      Await.result(webpage.loaded.future, 2 second)
    } catch {
      case timeout: TimeoutException => assert(false, "It should be cancallable.")
    }
  }

}