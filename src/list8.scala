import scala.reflect.ClassTag

class FullException(msg: String) extends Exception(msg)

abstract class MyQueue[E] {
  @throws[FullException]
  def enqueue(x: E): Unit
  def dequeue: Unit

  @throws[NoSuchElementException]
  def first: E
  def isFull: Boolean
  def isEmpty: Boolean

}

class QueueMut[E: ClassTag](val capacity: Int = 1000) extends MyQueue[E]{

  private val size=capacity +1
  private val q: Array[E] = new Array[E](size)
  private var f, r  = 0

  override def enqueue(x:E):Unit=
    if (!this.isFull)
    {
      q(r) = x
      r = (r + 1) % size
    }
    else
      throw new FullException("Queue is full")

  override def dequeue: Unit =
    if (!this.isEmpty)
     { q(f) = null.asInstanceOf[E]
      f = (f + 1) % size
     }

  override def first: E =
    if(!this.isEmpty)
      q(f)
    else
      throw new NoSuchElementException("Queue is empty")

  override def isEmpty: Boolean =
    f == r

  override def isFull: Boolean =
    f == ((r + 1) % size)

  override def toString: String =
    q.toList.mkString(", ") + "     f = " + f + "  r = " + r

}

object QueueMut {
  def apply[E: ClassTag](xs: E*) =
    xs.foldLeft[QueueMut[E]](new QueueMut[E](xs.length))((q,elem)=> {q.enqueue(elem);q})

  def empty[E: ClassTag](capacity: Int = 1000): QueueMut[E] = new QueueMut[E](capacity)
}

object QueueMutTest {

  def main(args: Array[String]): Unit = {
    var q = new QueueMut[Int](3)
    var q2: QueueMut[Int] = QueueMut.empty(3)
    println(q2.dequeue)
    try {
      println(q2.first)
    }catch
      {case e: NoSuchElementException => println("Empty Queue Exception")}
    q.enqueue(1)
    println(q.toString)
    q.enqueue(2)
    println(q.toString)
    println(q.first)

    q.dequeue
    println(q.first)
    q.enqueue(3)
    println(q)
    q.enqueue(4)
    println(q.toString)

    try {
      println(q.enqueue(5))
    }catch
      {case e: FullException => println("Full Queue Exception")}

    q.dequeue
    q.enqueue(5)
    println(q.toString)

    q2.enqueue(1)
    q2.enqueue(3)
    println(q2.toString)
    q2.dequeue
    println(q2.toString)
    println(q2.first)

    var q3 = QueueMut(1, 2, 3)
    println(q3.first)

    try {
      println(q3.enqueue(5))
    }catch
      {case e: FullException => println("Full Queue Exception")}

    q3.dequeue
    println(q3.toString)
    q3.enqueue(5)
  }
}