//zad1a

def exists[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match {
    case x::h => p(x) || exists(h)(p)
    case Nil => false
  }
exists (List(5,1,2,3)) (_ == 2) == true
exists (List(5,1,3)) (_ == 2) == false
exists (Nil)(_==2)==false
exists (List(5,1.3,2,3)) (_ == 3) == true

//zad1b

def exists2[A](xs: List[A])( p: A => Boolean): Boolean =
  xs.foldLeft(false){_||p(_)}

exists2 (List(5,1,2,3)) (_ == 2) == true
exists2 (List(5,1,3)) (_ == 2) == false
exists2 (Nil)(_==2)==false
exists2 (List(5,1.3,2,3)) (_ == 3) == true

//zad1c

def exists3[A](xs: List[A])(p: A => Boolean): Boolean = xs.foldRight(false)(p(_)||_)

exists3 (List(5,1,2,3)) (_ == 2) == true
exists3 (List(5,1,3)) (_ == 2) == false
exists3 (Nil)(_==2)==false
exists3 (List(5,1.3,2,3)) (_ == 3) == true

//zad2

def filter[A](list: List[A])(p: A => Boolean): List[A] =
  list.foldRight(List[A]()) { (r,c) =>
    if( p(r) ) r :: c else c }

filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filter (List(2,7,1,3,7,8,4,1,6,9)) (_ < 3) == List(2,1,1)
filter (List(2,2))(_!=2)==  List()
filter(List())(_!=2)==List()

//zad3a

def remove1 [A](xs: List[A])(p: A => Boolean): List[A] =
  xs match {
    case h::t => if(p(h)) t else h::remove1(t)(p)
    case Nil => Nil
  }


remove1(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1(List(1,2,3,2,5)) (_ == 3) == List(1, 2, 2, 5)
remove1(List(1)) (_ == 1) == List()
remove1(List()) (_ == 1) == List()
remove1(List("a","b","c","d","e")) (_ == "a") == List("b","c","d","e")

//zad3b

def remove2 [A](xs: List[A])(p: A => Boolean): List[A] =
{
  def remove_h[A](xs: List[A], p: A => Boolean, acc: List[A]): List[A] =
    xs match {
      case h :: t => if (p(h)) acc reverse_:::t else remove_h(t, p, h :: acc)
      case Nil => acc.reverse
    }
  remove_h(xs,p,Nil)
}

remove2(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove2(List(1,2,3,2,5)) (_ == 3) == List(1, 2, 2, 5)
remove2(List(1)) (_ == 1) == List()
remove2(List()) (_ == 1) == List()
remove2(List("a","b","c","d","e")) (_ == "a") == List("b","c","d","e")

//zad4

def splitAt [A](xs: List[A])(n: Int): (List[A], List[A])= {
  def spiltH[A](xs: List[A])(acc1: List[A])(n: Int): (List[A], List[A]) =
    xs match {

      case h :: t => if ( n>0) spiltH(t)(h::acc1) (n-1) else (acc1.reverse, xs)
      case Nil => (acc1.reverse, Nil)
    }
  spiltH(xs)(Nil)(n)
}

splitAt (List('a','b','c','d','e')) (2) == (List('a', 'b'), List('c', 'd', 'e'))
splitAt (List()) (2) == (List(), List())
splitAt (List('a','b','c','d','e')) (6) == (List('a', 'b','c', 'd', 'e'),List())
splitAt (List('a','b','c','d','e')) (-3) == (List(),List('a', 'b','c', 'd', 'e'))