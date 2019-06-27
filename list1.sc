//zad1
def suma(xs:List[Double]):Double={
  if(xs==Nil) 0.0
  else xs.head+suma(xs.tail)

}
suma(List())==0
suma(List(1.5,1.5))==3
suma(List(5.6))==5.6
suma(List(-1, 2, 3)) == 4.0
suma(Nil)==0.0

//zad2
def ends[A](xs : List[A]) = {
  if(xs==Nil) throw new NoSuchElementException("pusta lista")
  def last[A](xs:List[A]): A=
    if(xs.tail==Nil) xs.head
    else last(xs.tail)

  (xs.head,last(xs))
}

//ends(List())
ends(List(3))==(3,3)
ends(List(5,6))==(5,6)
ends(List(-1, 2, 3)) == (-1,3)


//zad3
val posortowana: List[Int] => Boolean = xs=>
  xs==Nil || xs.tail==Nil || (xs.head<xs.tail.head && posortowana(xs.tail))

posortowana(List())
posortowana(List(5))
posortowana(List(-1, 2, 3))
posortowana(List(-1, 6, 3))
posortowana(List(-1, 6))

//zad4
def glue (xs: List[String], sep: String):String=
 if( xs==Nil) ""
 else if(xs.tail==Nil) xs.head
 else xs.head+sep+glue(xs.tail,sep)

glue(List("To", "jest", "napis"), "-")=="To-jest-napis"
glue(Nil,"-")==""
glue(List("Anna"),"-")=="Anna"