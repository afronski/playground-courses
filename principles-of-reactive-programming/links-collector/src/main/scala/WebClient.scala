package linksCollector

import java.util.concurrent.Executor

import scala.concurrent.{ Future, Promise }

import com.ning.http.client.AsyncHttpClient

object WebClient {
  case class BadStatus(code: Int) extends Throwable {
    require(code >= 200 && code < 600)
  }

  private val client = new AsyncHttpClient

  def get(url: String)(implicit executor: Executor): Future[String] = {
    val future = client.prepareGet(url).execute()
    val promise = Promise[String]()

    future addListener(new Runnable {
      def run = {
        val response = future.get

        if (response.getStatusCode < 400) {
          promise.success(response.getResponseBodyExcerpt(131072))
        } else {
          promise.failure(BadStatus(response.getStatusCode))
        }
      }
    }, executor)

    promise.future
  }
}