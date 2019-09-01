
trait List[T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def toString: String
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("empty list has no head")
  def tail: Nothing = throw new NoSuchElementException("empty list has no tail")
  override def toString: String = "()"
}


class Cons[T](val head:T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString = s"( $head, $tail)"
}

val moose = new Cons(3,new Nil())
moose

//object ListO {
//  def List0[T] = new Nil[T]
//  def List1[T](x:T) = new Cons[T](x, List0[T])
//  def List2[T](x:T,y:T) = new Cons[T](x, List1[T](y))
//}
object List {
  def apply[T](x1:T, x2:T) = new Cons[T](x1,new Cons(x2,new Nil[T]))
  def apply[T] (x1:T) = new Cons[T](x1,new Nil[T])
  def apply() = new Nil[Nothing]()

}

val dog = List(3,4)
println(dog)

val catdog = List()
println(catdog)

val dogcat = List(2)
println(dogcat)