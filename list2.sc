import scala.annotation.tailrec
//zad1

def take[A](n:Int,xs:List[A]):List[A]=
  xs match
  {
    case h::t=> if(n>0) h::take(n-1,t) else Nil
    case Nil=>Nil
  }

take(2, List(1,2,3,5,6)) == List(1,2)
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)
take(2,Nil)==Nil
take(2,List(3))==List(3)
//zad2
def drop[A](n:Int, xs:List[A]):List[A]=
  xs match
  {
    case h::t=> if(n>0) drop(n-1,t) else xs
    case Nil=>Nil
  }

drop(2, List(1,2,3,5,6)) == List(3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(8, List(1,2,3,5,6)) == Nil
drop(1,Nil)==Nil

//zad3
def reverse[A](xs:List[A])={
  def rev[A](acc:List[A],xs:List[A]):List[A]=
   xs match
   {
    case h::t=>rev(h::acc,t)
    case Nil=>acc
   }
  rev(Nil,xs)
}

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(Nil)==Nil
reverse(List("Ala"))==List("Ala")

//zad4
def replicate(xs:List[Int]):List[Int]=
{
  def rep(acc:List[Int], n:Int, x:Int):List[Int]=
    if(n>0) rep(x::acc,n-1,x) else acc

  xs match
  {
    case h::t=>rep(replicate(t),h,h)
    case Nil=>Nil
  }
}
replicate (List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(Nil)==Nil
replicate(List(2))==List(2,2)

//zad5
def root(a:Double):Double=
{
  def root_h(x:Double, ep:Double):Double=
    if (math.abs((x*x*x)-a)> ep*math.abs(a)) root_h(x+(((a/(x*x))-x)/3),ep) else x

  root_h(if (a>1) a/3 else a,10E-15)
}
math.abs(root(-8.0) +2.0)<= math.pow(10,-70)
math.abs(root(8.0) -2.0)<= math.pow(10,-70)
math.abs(root(0.0) -0.0)<= math.pow(10,-70)
math.abs(root(1.0) -1.0)<= math.pow(10,-70)
math.abs(root(-1.0) +1.0)<= math.pow(10,-70)
math.abs(root(278.0) -6.5265188)<= math.pow(10,-6)
println(math.abs(root(278.0)))