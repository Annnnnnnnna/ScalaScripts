var count = 0

//zad1
def whileLoop2( condition: =>Boolean)(e: =>Unit): Unit=
  if(condition) {
    e
    whileLoop2(condition)(e)
  }

whileLoop2(count < 5) {
  println(count)
  count += 1
}

//zad2
def lrepeat [A] (k: Int) (stream: Stream[A]): (Stream[A])=
{
  def rep[A](acc: =>Stream[A], n:Int, x:A): Stream[A]=
    if(n>0) rep(x#::acc,n-1,x) else acc

  stream match
  {
    case h#::t=>rep(lrepeat(k)(t),k,h)
    case Stream.Empty=>Stream.Empty
  }
}

(lrepeat (3) (Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
lrepeat(3)(Stream.Empty).toList ==List()
lrepeat(2)(Stream('a','b','c')).toList==List('a','a','b','b','c','c')

//zad3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//zad3a
def lBreadth[A](ltree: lBT[A]) : Stream[A] = {
  def search[A](xs: List[lBT[A]]) : Stream[A] = {
    xs match  {
      case LNode(elem, left, right)::t => elem #:: search(t ::: List(left(), right()))
      case LEmpty :: t => search(t)
      case Nil => Stream.Empty
    }
  }
  search(List(ltree))
}

val t = LNode(1,()=> LNode(2,()=> LEmpty,()=> LNode(3,()=> LEmpty,()=>LEmpty)),()=> LEmpty)
lBreadth(t).toList==List(1,2,3)
lBreadth(LEmpty).toList==List()

//zad3b
def  lTree (n: Int):lBT[Int]=
{
  LNode(n,()=>lTree(2*n),()=>lTree(2*n+1))
}

val t1= lTree(2)
lBreadth(t1).take(12).toList == List(2, 4, 5, 8, 9, 10, 11, 16, 17, 18, 19, 20)


lBreadth(lTree(1)).take(10).toList == List(1,2,3,4,5,6,7,8,9,10)