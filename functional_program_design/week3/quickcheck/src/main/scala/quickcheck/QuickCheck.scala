package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

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

}
