package nodescala

import java.util.regex.Pattern

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

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]
    intercept[TimeoutException] {
      Await.result(never, 1 second)
    }
  }

  test("All should be successful when there is only a success") {
    val success = Future.always("success")
    assert(Await.result(Future.all(List(success)), 1 second) == List("success"))
  }

  test("All should be successful when there are only success'") {
    val futures = List(
      Future.always("first success"),
      Future.always("second success")
    )
    assert(Await.result(Future.all(futures), 1 second) == List("first success", "second success"))
  }

  test("All should wait for all success to arrive") {
    val futures = List(
      Future { Thread.sleep(100); "first success" },
      Future.always("second success")
    )
    assert(Await.result(Future.all(futures), 1 second) == List("first success", "second success"))
  }

  test("All should fail when there is only a failure'") {
    val futures = List(
      Future.failed(new Exception("failed future"))
    )
    val t = intercept[Exception] {
      Await.result(Future.all(futures), 1 second)
      fail()
    }
    assert(t.getMessage == "failed future")
  }

  test("All should fail when there are one failure and a success") {
    val futures = List(
      Future.failed(new Exception("failed future")),
      Future.always("success")
    )
    val t = intercept[Exception] {
      Await.result(Future.all(futures), 1 second)
      fail()
    }
    assert(t.getMessage == "failed future")
  }

  test("Any should be successful when it completes first") {
    val success = Future.always("success")
    val failure = Future.never[String]

    assert(Await.result(Future.any(List(success, failure)), 1 millis) == "success")
  }

  test("Any should be a failure when it completes first") {
    val success = Future { Thread.sleep(100); "success" }
    val failure = Future.failed(new Exception("failed future"))
    val t = intercept[Exception] {
      Await.result(Future.any(List(success, failure)), 1 millis)
    }
    assert(t.getMessage == "failed future")
  }

  test("Any should be the first successful future") {
    val futures = List(
      Future { Thread.sleep(100); "first success" },
      Future { Thread.sleep(50); "second success" }
    )
    assert(Await.result(Future.any(futures), 1 second) == "second success")
  }

  test("Delay should wait for at least the duration") {
    val delay = Future.delay(100 millis)
    val exp = intercept[TimeoutException] {
      Await.result(delay, 100 millis)
    }
    assert(exp.getMessage.contains("Futures timed out after"))
  }

  test("Delay should wait for at most the duration") {
    val delay = Future.delay(100 millis)
    Await.result(delay, 105 millis)
    assert(true)
  }

  test("Run should cancel a running computation") {
    val promise = Promise[String]()
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {}
        fail
      }
    }
    Future.delay(500 millis) onSuccess {
      case _ => {
        working.unsubscribe()
        promise.trySuccess("cancelled")
      }
    }
    assert(Await.result(promise.future, 1 second) == "cancelled")
  }

  test("Now should return the value when it's ready") {
    val future = Future.always("success")
    assert(future.now == "success")
  }

  test("Now should throw NoSuchElementException itsn't ready") {
    val future = Future.never
    intercept[NoSuchElementException] {
      future.now
    }
  }

  test("ContinueWith should continue after the future") {
    val f1 = Future[Int] { 1 }
    val f2 = (f: Future[Int]) => { f.now + 1 }
    val result = Await.result(f1.continueWith(f2), 1 second)
    assert(result == 2)
  }

  test("ContinueWith should continue after the future is completed") {
    val f1 = Future[Int] { Thread.sleep(200); 1 }
    val f2 = (f: Future[Int]) => { f.now + 1 }
    val result = Await.result(f1.continueWith(f2), 1 second)
    assert(result == 2)
  }

  test("Continue should continue after the future") {
    val f1 = Future[Int] { 1 }
    val f2 = (f: Try[Int]) => { f.get + 1 }
    val result = Await.result(f1.continue(f2), 1 second)
    assert(result == 2)
  }

  test("Continue should continue only after the future is completed") {
    val f1 = Future[Int] { Thread.sleep(200); 1 }
    val f2 = (f: Try[Int]) => { f.get + 1 }
    val result = Await.result(f1.continue(f2), 1 second)
    assert(result == 2)
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
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
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
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
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

}




