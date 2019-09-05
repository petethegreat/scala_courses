package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    //PT
    val testLeafTreeList = List(Leaf('a',1), Leaf('b',2), Leaf('c',3), Leaf('d',4))
    val testDecodeTree = Fork(Leaf('a',6),Fork(Leaf('b',3),Leaf('c',1),"bc".toList,4),"abc".toList,10)
	}
  // PT
  trait TestCharacters{
    val mySet: Set[(Char, Int)] = Set(('e',1), ('w',1), ('h',1), ('r',1), ('d',1), (' ',1))
    val myString: String = "hello world"


  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  // PT
  test("PT - test character frequencies") {
    assert( times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')).toSet === Set(('e',1), ('w',1), ('h',1), ('r',1), ('d',1), (' ',1), (',', 1), ('l',3),('o',2) ))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  //PT
  test( "test until") {
    new TestTrees { assert(until(singleton,combine)(testLeafTreeList) == List(Fork(Leaf('d',4),Fork(Fork(Leaf('a',1), Leaf('b',2),"ab".toList,3), Leaf('c',3),"abc".toList,6), "dabc".toList,10)))
    }
  }
// PT test decode
  test( "PT - test decode") {
    new TestTrees {
      assert(decode(testDecodeTree,List(0,1,0,1,1)) === "abc".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  //PT
  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abdbbadbdbdabbbbda".toList)) === "abdbbadbdbdabbbbda".toList)
    }
  }

}
