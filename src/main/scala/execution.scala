import java.util.concurrent.Executor

import org.asynchttpclient.DefaultAsyncHttpClientConfig.Builder
import org.asynchttpclient._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait HttpExecutor {
  def client: AsyncHttpClient

  def apply(req: Req)

  def apply[T](pair: (Request, AsyncHandler[T]))
              (implicit executor: ExecutionContext): Future[T] =
    apply(pair._1, pair._2)

  def apply[T] (request: Request, handler: AsyncHandler[T])(implicit executor: ExecutionContext): Future[T] = {
    val lfut = client.executeRequest(request, handler)
    val promise = scala.concurrent.Promise[T]()
    lfut.addListener(() => promise.complete(Try(lfut.get())),
      new Executor {
        override def execute(command: Runnable): Unit = {
          executor.execute(command)
        }
      }
    )
    promise.future
  }

  def shutdown() = {
    client.close()
  }
}

// Http executor with defaults
case class HTTP(clientBuilder: DefaultAsyncHttpClientConfig.Builder) extends HttpExecutor {
  lazy val client = new DefaultAsyncHttpClient(clientBuilder.build)

  def closeAndConfigure(withBuilder: Builder => Builder): HTTP = {
    client.close()
    val newBuilder = new Builder(this.clientBuilder.build)
    copy(clientBuilder = withBuilder(newBuilder))
  }
}

object HTTP {
  val defaultClientBuilder = InternalDefaults.clientBuilder

  lazy val default: HTTP = {
    HTTP(defaultClientBuilder)
  }

  def withConfiguration(withBuilder: Builder => Builder): HTTP = {
    val newBuilder = new Builder(defaultClientBuilder.build)
    HTTP(withBuilder(newBuilder))
  }
}