import java.util.concurrent.{ArrayBlockingQueue, Semaphore}
import scala.concurrent.ExecutionContext

class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
  override def run: Unit =
    for(i <- 1 to 10) {
     println(s"$getName producing $i"); buf.put(i)
    }
}

class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
  override def run =
    for (_ <- 1 to 10)
      println(s"$getName consumed ${buf.take}")
}

object prodCons {

  def main(args: Array[String]) {
    val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    new Producer("Producer", buf).start
    new Consumer("Consumer", buf).start

val buf2: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    var producers = 2
    var cons = 2
    for (i <- 1 to producers) { new Producer(s"Producer$i", buf2).start }
    for (i <- 1 to cons) { new Consumer(s"Consumer$i", buf2).start }
    println("producers and consumers")
  }
}

object executeContextPros {

  def produce(name: String,buf: ArrayBlockingQueue[Int]) =
    for(i <- 1 to 10) { println(s"${name} producing $i"); buf.put(i) }

  def consume(name: String,buf: ArrayBlockingQueue[Int]) =
    for (_ <- 1 to 10) println(s"${name} consumed ${buf.take}")

  def main(args: Array[String]): Unit = {

    val buf = new ArrayBlockingQueue[Int](5)
    val ctx = ExecutionContext.global

    ctx.execute(() => produce("Producer",buf))
    ctx.execute(() => consume("Consumer",buf))

    Thread.sleep(500)

    println("producers and consumers")
    var producers = 2
    var consumers = 3
    val buf2: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)

    for(i <- 1 to producers)
      ctx.execute(() => produce(s"Producer$i",buf2))

    for(i <- 1 to consumers)
      ctx.execute(() => consume(s"Consumer$i",buf2))

    Thread.sleep(500)

    producers = 2
    consumers = 2
    val buf3: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    for(i <- 1 to producers)
      ctx.execute(() => produce(s"Producer$i",buf3))

    for(i <- 1 to consumers)
      ctx.execute(() => consume(s"Consumer$i",buf3))
  }
}

class Table(seats: Int = 5) {

  var chopstick = List.fill(seats)(new Semaphore(1))
  var guard = new Semaphore(seats - 1)

  def eat(id: Int) = {
    println(s"Philosopher $id waits to enter the dining room")

    val stick1 = id
    val stick2 = (id + 1) % seats

    guard.acquire()

    chopstick(stick1).acquire()
    chopstick(stick2).acquire()

    println(s"Philisopher $id is eating")
    Thread.sleep(100)

    chopstick(stick1).release()
    chopstick(stick2).release()

    guard.release()
    println(s"Philosopher $id leaves the dining room and goes to meditate")
  }
}

object Philosphers {

  def main(args: Array[String]): Unit = {
    var philosophers = 5
    val table = new Table(philosophers)
    for( i <- 0 until philosophers) {
      println(s"Summoning philosopher $i")
      new Thread(() => for(_ <- 0 to 3) {
        println(s"Philosopher $i meditate")
        Thread.sleep(1000)
        table.eat(i)
      }).start()
    }
  }
}