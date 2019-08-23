

// int sets

abstract class IntSet{
  def contains(x:Int): Boolean
  def incl(x: Int): IntSet
}

// abstract class cannot be instantiated
//val moose = new IntSet

class Empty extends IntSet {
  def contains( elem: Int): Boolean = false
  def incl(x:Int): IntSet = new NonEmpty(x,new Empty, new Empty)

}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  def incl(x:Int): IntSet = if (x < elem) new NonEmpty(x,left)
  else if (x > elem)
    else this

}