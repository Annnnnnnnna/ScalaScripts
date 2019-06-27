import scala.concurrent._
import scala.concurrent.Future
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global
import scala.io.Source

object Lista11 {
  def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    fut1 zip fut2

  def pairFut2[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for {
      a <- fut1
      b <- fut2
    } yield (a, b)

  implicit class FutureOps[T](val self: Future[T]) {
    def or(that: Future[T]): Future[T] = {
      val p = Promise[T]
      self onComplete { case x => p tryComplete x }
      that onComplete { case y => p tryComplete y }
      p.future
    }

    def exists(p: T => Boolean): Future[Boolean] = {
      val promise = Promise[Boolean]
      self onComplete {
        case Success(value) => promise.success(p(value))
        case Failure(error) => promise.success(false)
      }
      promise.future
    }

    def exists2(p: T => Boolean): Future[Boolean] =
      self map { v => p(v) } recover { case _ => false }
  }

  def main(args: Array[String]): Unit = {
    val pair = pairFut(Future {
      "success"
    }, Future {
      120 + 5
    })
    val pair2 = pairFut(Future {
      "fail"
    }, Future {
      1 / 0
    })
    Thread.sleep(500)
    println(pair.value)
    println(pair2.value)
    println()

    val pair12 = pairFut(Future {
      "success"
    }, Future {
      120 + 5
    })
    val pair22 = pairFut(Future {
      "fail"
    }, Future {
      1 / 0
    })
    Thread.sleep(500)
    println(pair12.value)
    println(pair22.value)
    println()

    val f1_success = Future {
      1
    }.exists(_ > 0)
    val f1_fail = Future {
      -1
    }.exists(_ > 0)
    val f1_fail2 = Future {
      1 / 0
    }.exists(_ > 1)
    Thread.sleep(500)
    println(f1_success.value)
    println(f1_fail.value)
    println(f1_fail2.value)
    println()

    val f2_success = Future {
      1
    }.exists2(_ > 0)
    val f2_fail = Future {
      -1
    }.exists2(_ > 0)
    val f2_fail2 = Future {
      1 / 0
    }.exists2(_ > 1)
    Thread.sleep(500)
    println(f2_success.value)
    println(f2_fail.value)
    println(f2_fail2.value)
    println()
  }
}
  object WordCount {
    def main(args: Array[String]) {
      val path = "lista11_/"
      val promiseOfFinalResult = Promise[Seq[(String, Int)]]

      promiseOfFinalResult.future onComplete {
        case Success(result) => result foreach println
        case Failure(t) => t.printStackTrace
      }
      Thread.sleep(5000)

      val futureOfProcessFiles = Promise[Seq[(String, Int)]]
      futureOfProcessFiles.future onComplete {
        case Success(success) => promiseOfFinalResult.success(success)
        case Failure(error) => println(s"Error processFiles() : $error")
      }

      scanFiles(path) onComplete {
        case Success(fileNames) => {
          processFiles(fileNames) onComplete {
            case Success(success) => futureOfProcessFiles.success(success)
            case Failure(error) => print(error)
          }
        }
        case Failure(error) => println(s"Error scanFiles(): $error")
      }
      Thread.sleep(5000)
    }

    private def processFiles(fileNames: Seq[String]): Future[Seq[(String, Int)]] =
       Future.sequence(fileNames.map(file_name => processFile(file_name))) map {_.sortBy(_._2)}

    private def processFile(fileName: String): Future[(String, Int)] =
    {
      val file_ = Source.fromFile(fileName)
      try
        Future{ (fileName,file_.getLines().foldLeft(0)((acc,x)=>acc+x.split(" ").length))}
    }

    private def scanFiles(docRoot: String): Future[Seq[String]] =
      Future { new java.io.File(docRoot).list.map(docRoot + _) }
  }