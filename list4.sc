sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]
val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)

//za1
def sumBT(bt: BT[Int]):Int=
    bt match {
      case Node(el, tl, tr) => el + sumBT(tl) + sumBT(tr)
      case Empty => 0
    }


val tree1 = Node(1,Empty,Empty)
val tree2 = Node(2,Node(9,Empty,Node(2,Empty,Empty)),Node(3,Node(4,Empty,Empty),Empty))

sumBT(t) == 6
sumBT(tree1)==1
sumBT(tree2)==20
sumBT(Empty)==0

//zad2
def foldBT [A, B](f: A => ((B, B) => B))(acc: B)(bt: BT[A]):B=
  bt match {
    case Node(value, left, right)=> f (value)(foldBT (f) (acc) (left),foldBT(f)(acc)(right))
    case Empty => acc
  }

//zad3a
def sumBT2[A](bt: BT[Int]):Int= foldBT ((x: Int)=>(y:Int,z:Int)=>x +y+z) (0) (bt)

sumBT2(t) == 6
sumBT2(tree1)==1
sumBT2(tree2)==20
sumBT2(Empty)==0

//zad3b
def inorderBT[A](bt: BT[A]):List[A]= foldBT[A,List[A]]((x)=>(y,z)=>y:::x::z)(acc=List())(bt)
inorderBT(t)==List(2, 3, 1)
inorderBT(tree1)== List(1)
inorderBT(tree2)==List(9,2,2,4,3)
inorderBT(Empty)==List()

//zad4
def mapBT2[A, B](f: A => B)(tree: BT[A]):BT[B]=
  foldBT [A,BT[B]](e1=>(y,z)=>Node(f(e1), y,z))(acc =Empty)(tree)


mapBT2((v: Int) => 2 * v)(t: BT[Int]) == Node(2,Node(4,Empty,Node(6,Empty,Empty)),Empty)
mapBT2((x:Int) => 2*x) (tree2: BT[Int])== Node(4,Node(18,Empty,Node(4,Empty,Empty)),Node(6,Node(8,Empty,Empty),Empty))
mapBT2((x:Int)=>2*x)(tree1:BT[Int])==Node(2,Empty,Empty)
mapBT2((x:Int)=>2*x)(Empty:BT[Int])==Empty

//zad5
sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0,2,4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0,2)
    case n => throw
      new NoSuchElementException("Graph g: node" + n +" doesn't exist")
  })

def pathExists[A](g: Graph[A])(from: A, to: A):Boolean=
{
    def search(visited: List[A]) (toVisit: List[A]): Boolean =
      toVisit match {
        case h::t => h == to || (if(visited contains h) search(visited)(t) else search(h::visited)(t ::: (g succ h)))
        case Nil => false
      }
    search (Nil) (List(from))
  }
pathExists(g)(4,1)
pathExists(g)(0,4)