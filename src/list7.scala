class UnderflowException(msg: String) extends Exception(msg)

class MyQueue[+T] private (private val xs: List[T], private val ys: List[T]) {
  def this() = this(List(), List())
  def isEmpty= xs==Nil

  def first = xs match {
    case x::_ => x
    case Nil => throw new UnderflowException("Empty queue")
  }

  def enqueue[B >: T](x:B )=
  if(xs!=Nil)
    new MyQueue(xs, x::ys)
  else
    new MyQueue(x::xs,ys)

  def dequeue() =
    xs match {
      case _::Nil => new MyQueue(ys.reverse, Nil)
      case _::t => new MyQueue(t,ys)
      case Nil => this
    }

  def firstOption()=
    xs match
    {
      case h::_ => Some(h)
      case Nil=>None
    }
  override def toString: String =if(isEmpty) "empty" else {(xs ::: ys.reverse) mkString ", "}
}
object MyQueue {
  def empty[T]= new MyQueue[T](Nil,Nil)
  def apply[T](xs:T*)= new MyQueue[T](xs.toList, Nil)
}

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

object QueueTest {

  val t = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty)
  val tt = Node(1,
    Node(2,
      Node(4,
        Empty,
        Empty
      ),
      Empty
    ),
    Node(3,
      Node(5,
        Empty,
        Node(6,
          Empty,
          Empty
        )
      ),
      Empty
    )
  )
  def main(args: Array[String]): Unit = {

    var queue = MyQueue(1,2,3)
    var q1 = MyQueue('a', 'b', 'c')
    var queuEmpty = MyQueue()
    var q5 = new MyQueue
    var q6 = MyQueue.empty

    println(queue.isEmpty)
    println(queuEmpty.isEmpty)
    println(queuEmpty.toString)
    println(queue.toString)
    println(queuEmpty.enqueue(1).toString)
    println(queue.enqueue(1).toString)
    println(q5.enqueue(2).toString)
    println(q6.enqueue("a").toString)
    println(q1.toString)
    q6.dequeue()

    println(q5.dequeue().toString)
    println(q5.dequeue().toString)

    println(q5.firstOption())
    println(queue.firstOption())
    try {
      println(q5.first)
    }catch
      {case e: UnderflowException => println("Empty Queue Exception")}
    println(queue.first)

    println(q5.enqueue(2).enqueue(1).dequeue())
    println(q6.enqueue(2).dequeue().enqueue(1))

    q5.dequeue()
    q6.dequeue()
    println(q5.enqueue(2).enqueue(1).first)
    println(q6.enqueue(2).first)

    println(breadthBT(t))
    println(breadthBT(tt))
    println(breadthBT(Empty))

  }
  def breadthBT[A](tree: BT[A]) : List[A] =  {

    def breadth(toVisit: MyQueue[BT[A]]) : List[A] = {
      toVisit.firstOption() match {
        case Some(Node(elem, left, right)) => {elem :: breadth(toVisit.dequeue().enqueue(left).enqueue(right))}
        case Some(Empty) =>breadth(toVisit.dequeue())
        case _ => Nil
      }
    }
    breadth(MyQueue(tree))
  }
}