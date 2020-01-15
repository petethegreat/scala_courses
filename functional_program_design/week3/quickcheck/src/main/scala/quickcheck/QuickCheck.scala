package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
// PT


// def sizedheapGen(size:Int) : Gen[H]
// https://booksites.artima.com/scalacheck/examples/html/ch06.html
// https://stackoverflow.com/questions/3666744/sized-generators-in-scalacheck
// https://alvinalexander.com/scala/scalacheck-custom-generator-examples
// https://alvinalexander.com/scala/fp-book/quick-review-scala-for-expressions
// https://www.scala-exercises.org/scalacheck/generators


  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for { 
    	i <- arbitrary[Int]
    	// m <- frequency((1,const(H.empty)),(4,genHeap))
    	m <- oneOf(const(empty),genHeap)
    } yield insert(i,m)
)

  //
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert2min") = forAll { (a:Int, b:Int) =>
    val h = insert(b,insert(a,empty))
    findMin(h) == Math.min(a,b)
  }

  property("insert_empty_delete_min") = forAll { a:Int =>
    isEmpty(deleteMin(insert(a,empty)))
  }

//  def getMinSequence(theheap:H):List[A] =
//    theheap match {
//      case empty => Nil;
//      case _ => findMin(theheap)::getMinSequence(deleteMin(theheap))}

  def getMinSequence(theheap:H):List[A] =
    if (isEmpty(theheap)) Nil else findMin(theheap)::getMinSequence(deleteMin(theheap))


  property("findMin_gives_sorted_sequence") = forAll { h:H =>
    lazy val thelist = getMinSequence(h)
    thelist == thelist.sorted
  }

  property("melded_min_is_one_of_component_mins") = forAll { (h: H, g: H) =>
    val mins = List(g, h).filter(x => !isEmpty(x)).map(x => findMin(x))
    if (mins.length == 0) true else mins.contains(findMin(meld(g, h)))
  }

  property("min_after_deletion_is_greater") = forAll{ (h:H) =>
    h match {
      case x::x1::xs => findMin(h) <= findMin(deleteMin(h))
      case _ => true}
  }

  property("insertions_preserved_for_non_min") = forAll{ (a:Int,b:Int) =>
    findMin(deleteMin(insert(b,insert(a,empty)))) == Math.max(a,b)
  }

  property("sorted_after_melding") = forAll{ (h:H, g:H) =>
    getMinSequence(meld(g,h)) == (getMinSequence(g):::getMinSequence(h)).sorted

  }






  // test properties





}
