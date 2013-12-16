package suggestions

import language.postfixOps

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.{ Try, Success, Failure }

import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )

    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("recovered() should behave as it suppose") {
    val observable = Observable(1, 2, 3, 4)

    val response = observable.map(num => if (num != 4) num else throw new Exception())
    val sequence = response.recovered

    val sum = sequence.foldLeft((0, 0)) { (accumulator, tried) =>
      tried match {
        case Success(_) => (accumulator._1 + 1, accumulator._2)
        case Failure(_) => (accumulator._1, accumulator._2 + 1)
      }
    }

    var result = (0, 0)
    sum.subscribe(event => result = event)

    assert(result._1 == 3, s"It should have only 3 success cases (has ${result._1}).")
    assert(result._2 == 1, s"And it should have only one failure (has ${result._2}).")
  }

  test("recovered() with two values") {
    val observable = Observable(1, 2)
    val sequence = observable.recovered

    val sum = sequence.foldLeft((0, 0)) { (accumulator, tried) =>
      tried match {
        case Success(_) => (accumulator._1 + 1, accumulator._2)
        case Failure(_) => (accumulator._1, accumulator._2 + 1)
      }
    }

    var result = (0, 0)
    sum.subscribe(event => result = event)

    assert(result._1 == 2, s"It should have only 2 success cases (has ${result._1}).")
    assert(result._2 == 0, s"And it should have no failure (has ${result._2}).")
  }

  test("recovered() with one value and one failure") {
    val observable = Observable(1, 2)

    val response = observable.map(num => if (num != 2) num else throw new Exception())
    val sequence = response.recovered

    val sum = sequence.foldLeft((0, 0)) { (accumulator, tried) =>
      tried match {
        case Success(_) => (accumulator._1 + 1, accumulator._2)
        case Failure(_) => (accumulator._1, accumulator._2 + 1)
      }
    }

    var result = (0, 0)
    sum.subscribe(event => result = event)

    assert(result._1 == 1, s"It should have only 1 success case (has ${result._1}).")
    assert(result._2 == 1, s"And it should have only one failure (has ${result._2}).")
  }

  test("recovered() with one failure") {
    val observable = Observable(1)

    val response = observable.map(num => if (num == 1) throw new Exception() else num)
    val sequence = response.recovered

    val sum = sequence.foldLeft((0, 0)) { (accumulator, tried) =>
      tried match {
        case Success(_) => (accumulator._1 + 1, accumulator._2)
        case Failure(_) => (accumulator._1, accumulator._2 + 1)
      }
    }

    var result = (0, 0)
    sum.subscribe(event => result = event)

    assert(result._1 == 0, s"It should have no success case (has ${result._1}).")
    assert(result._2 == 1, s"And it should have only one failure (has ${result._2}).")
  }

  test("test recovered() with one failure") {
    val requests = Observable(3, 2, 1)
    val comp = requests.map(i => i / (i - 1))

    val theList = comp.recovered.map(_.isFailure).toBlockingObservable.toList

    assert(theList === List(false, false, true))
  }

  test("recovered() with one failure (at first) and then one success") {
    val observable = Observable(1, 2)

    val response = observable.map(num => if (num == 1) throw new Exception() else num)
    val sequence = response.recovered

    val sum = sequence.foldLeft((0, 0)) { (accumulator, tried) =>
      tried match {
        case Success(_) => (accumulator._1 + 1, accumulator._2)
        case Failure(_) => (accumulator._1, accumulator._2 + 1)
      }
    }

    var result = (0, 0)
    sum.subscribe(event => result = event)

    assert(result._1 == 0, s"It should have no success case (has ${result._1}).")
    assert(result._2 == 1, s"And it should have only one failure (has ${result._2}).")
  }

  test("timedOut() should behave as it suppose") {
    val observable = Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L)

    val sum = observable.foldLeft(0) { (accumulator, value) =>
      accumulator + value._1
    }

    var result = 0
    sum.subscribe(event => result = event)

    assert(result == 1, s"It should sum only first element (sum is equal to $result).")
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation

    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }

    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }

    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("concatRecovered() from ScalaDoc") {
    val observable = Observable(1, 2, 3).concatRecovered(num => Observable(num, num, num))
    val list = observable.take(9).toBlockingObservable.toList

    assert(list == List(Success(1), Success(1), Success(1),
                        Success(2), Success(2), Success(2),
                        Success(3), Success(3), Success(3)))
  }

  test("concatRecovered() should behave as it suppose") {
    val observable = Observable(1, 2, 3, 4, 5)

    val responseTransformer = (num: Int) => if (num != 4) Observable(num) else Observable(new Exception())
    val sequence = observable concatRecovered responseTransformer

    val sum = sequence.foldLeft((0, 0)) { (accumulator, tried) =>
      tried match {
        case Success(_) => (accumulator._1 + 1, accumulator._2)
        case Failure(_) => (accumulator._1, accumulator._2 + 1)
      }
    }

    var result = (0, 0)
    sum.subscribe(event => result = event)

    assert(result._2 == 1, s"And it should have only one failure (has ${result._2}).")
    assert(result._1 == 4, s"It should have 4 success cases (has ${result._1}).")
  }
}