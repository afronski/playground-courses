package nodescala

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._

import ExecutionContext.Implicits.global

import scala.async.Async.{async, await}

object Main {

  def main(args: Array[String]) {

    // Instantiate the server at 8191, relative path "/test",
    // and have the response return headers of the request.
    val myServer = new NodeScala.Default(8191)
    val myServerSubscription = myServer.start("/test") {
      request => for (keyValue <- request.iterator) yield (keyValue + "\n").toString
    }

    // Create a future that expects some user input `x`
    // and continues with a `"You entered... " + x` message.
    val userInterrupted: Future[String] = Future.userInput("").continue { "You entered... " + _ }

    // Create a future that completes after 20 seconds
    // and continues with a `"Server timeout!"` message.
    val timeOut: Future[String] = Future.delay(20 seconds).continue { _ => "Server timeout!" }

    // Create a future that completes when either 10 seconds elapse
    // or the user enters some text and presses ENTER.
    val terminationRequested: Future[String] = userInterrupted.continueWith {
      Await.result(_, 10 seconds)
    }

    // Unsubscribe from the server.
    terminationRequested onSuccess {
      case msg => myServerSubscription.unsubscribe
    }
  }

}