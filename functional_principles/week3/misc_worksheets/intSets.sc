
// int sets
abstract class IntSet{
  def contains(x:Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

// abstract class cannot be instantiated
//val moose = new IntSet

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true

  def incl(x:Int): IntSet = if (x < elem) new NonEmpty(elem,left.incl(x), right)
    else if (x > elem)  new NonEmpty(elem,left, right.incl(x))
    else this
  // if the element to be included is not greater than or less than the value of this node
  // then it is equal to the value of this node. We don't need to include it, just return this
  // we update left and right elements as we include things

  override def toString = s"{ ${left} ${elem} ${right} }"
  override def union(other: IntSet) = left.union(right).union(other).incl(elem)
}

//class Empty extends IntSet {
//  def contains( elem: Int): Boolean = false
//  def incl(x:Int): IntSet = new NonEmpty(x,new Empty, new Empty)
//  override def toString = "."
//
//
//}

// all Empty sets are identical, so just have a single instance, rather than multiple
// define empty set as an object. objects are singleton. Can't use new Empty anymore, there is only one Empty.
// Also note - singleton objects don't have member data, only (statis) class methods.

object Empty extends IntSet {
  def contains( elem: Int): Boolean = false
  def incl(x:Int): IntSet = new NonEmpty(x,this, this)
  override def toString = "."
  override def union(other:IntSet) = other

}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1.incl(7)
val list1 = List(1,2,3)
def ListToSet(thelist:List[Int]): IntSet = thelist.map((x:Int) => new NonEmpty(x,Empty,Empty)).reduce(((x:IntSet, y:IntSet) => x.union(y)))
val set1 = ListToSet(List(9,5,3,1,7))
val set2 = ListToSet(List(8,2,6,4))
val set3 = set1.union(set2)



// week 4 - subtyping and generics
val a: Array[NonEmpty] = Array(new NonEmpty(1,Empty, Empty))
val b: Array[IntSet] = a
b(0) = Empty
val s: NonEmpty = a(0)


