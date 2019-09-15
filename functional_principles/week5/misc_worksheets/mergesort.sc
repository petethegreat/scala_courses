
//Hello mergesort, my old friend

import scala.math.Ordering
def msort(xs:List[Int]): List[Int] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge( left:List[Int],right:List[Int]) : List[Int] = (left,right) match {
        case (x::xs,y::ys) => if (x < y) x::merge(xs,y::ys) else y::merge(x::xs,ys)
        case (xx,Nil) => xx
        case (Nil,yy) => yy
      }
    val (left,right) = xs.splitAt(n)
    merge(msort(left),msort(right))
  }
}


val myList:List[Int] = List(753,76,7,77,3,6,8,3,3,5,8,3,2,5)
println(myList)

println(msort(myList))
val myList2 = List("pie","cake","donut","croissant","tart")


def msort_dumb[T](xs:List[T])(lt: (T,T) =>Boolean): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge( left:List[T],right:List[T]) : List[T] = (left,right) match {
      case (x::xs,y::ys) => if (lt(x,y)) x::merge(xs,y::ys) else y::merge(x::xs,ys)
      case (xx,Nil) => xx
      case (Nil,yy) => yy
    }
    val (left,right) = xs.splitAt(n)
    merge(msort_dumb(left)(lt),msort_dumb(right)(lt))
  }
}


println(msort_dumb(myList2)((x,y) => x.compareTo(y)<0))

def msort_ord[T](xs:List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge( left:List[T],right:List[T]) : List[T] = (left,right) match {
      case (x::xs,y::ys) => if (ord.lt(x,y)) x::merge(xs,y::ys) else y::merge(x::xs,ys)
      case (xx,Nil) => xx
      case (Nil,yy) => yy
    }
    val (left,right) = xs.splitAt(n)
    merge(msort_ord(left)(ord),msort_ord(right)(ord))
  }
}

println(msort_ord(myList2))

def pack[T](xs:List[T]):List[List[T]] = xs match {
  case Nil => Nil
  case y1::ys => pack(ys) match
  { case Nil => List(y1)::Nil
    case z1::zs => if (z1.head==y1) (y1::z1)::zs else (List(y1))::z1::zs
  }
}

pack(List(1,1,2,2,2,3,3,3,2,2,1,2))

def pack2[T](xs:List[T]):List[List[T]] = xs match {
  case Nil => Nil
  case y1::ys => {
    val (matched,unmatched) = xs.span(m => m == y1)
    matched::pack(unmatched)
  }

}
pack2(List(1,1,2,2,2,3,3,3,2,2,1,2))
def rl[T](l:List[T]): List[(T,Int)] = {
  pack2(l).map(x => (x.head,x.length))
}
rl(List(1,1,2,2,2,3,3,3,2,2,1,2))



