package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import java.util.NoSuchElementException

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  /*
   * this tests if an element x is inserted to an empty heap then the minimum of the resulting heap is x
   */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  /*
   * ************ first test findMin *************
   */
  /*
   * this tests if an element y is inserted to a heap with one element x (y<x) then the minimum of the resulting heap
   * is y
   */
  property("min2") = forAll { a: Int =>
    val h1 = insert(2, empty)
    val h2 = insert(1, h1)
    findMin(h2) == 1
  }

  /*
   * this tests if an element y is inserted to a heap with one element x (x<y) then the minimum of the resulting heap
   * is still x
   */
  property("min3") = forAll { a: Int =>
    val h1 = insert(1, empty)
    val h2 = insert(2, h1)
    findMin(h2) == 1
  }

  /*
   * this test that the findMin correctly finds the minimum when three elements are inserted in ascending and descending order
   */
  property("min4") = forAll {
    a: Int =>
      val h1 = insert(1, empty)
      val h2 = insert(2, h1)
      val h3 = insert(3, h2)
      findMin(h3) == 1
      findMin(h2) == 1
      findMin(h1) == 1
      val h4 = insert(3, empty)
      val h5 = insert(2, h4)
      val h6 = insert(1, h5)
      findMin(h4) == 3
      findMin(h5) == 2
      findMin(h6) == 1
  }

  /*
   * this tests if the same element is inserted twice then the minimum of the heap doesn't change
   */
  property("min4") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = insert(a, h1)
    findMin(h2) == a
  }

  /*
   * this tests that min of an empty heap throws NoSuchElement exception
   */
  property("min5") = forAll { a: Int =>
    try {
      findMin(empty)
      false
    } catch {
      case e: NoSuchElementException => true
    }
  }

  /*
   * this tests that inserting 1,1,2 will give 1 as minimum
   */
  property("min6") = forAll {
    a: Int =>
      val h1 = insert(1, empty)
      val h2 = insert(1, h1)
      val h3 = insert(2, h2)
      findMin(h3) == 1
  }

  /*
   * this tests that inserting 2,2,1 will give 1 as minimum
   */
  property("min7") = forAll {
    a: Int =>
      val h1 = insert(2, empty)
      val h2 = insert(2, h1)
      val h3 = insert(1, h2)
      findMin(h3) == 1
  }

  /*
   * this tests that inserting 2,1,2 will give 1 as minimum
   */
  property("min8") = forAll {
    a: Int =>
      val h1 = insert(2, empty)
      val h2 = insert(1, h1)
      val h3 = insert(2, h2)
      findMin(h3) == 1
  }
  
    /*
   * this tests that inserting 1,2,1 will give 1 as minimum
   */
  property("min8") = forAll {
    a: Int =>
      val h1 = insert(1, empty)
      val h2 = insert(2, h1)
      val h3 = insert(1, h2)
      findMin(h3) == 1
  }

  /*
   * ******************* test isEmpty ********************
   */
  /*
   * this tests that isEmpty of an empty heap returns true
   */
  property("isEmpty1") = forAll { a: Int =>
    isEmpty(empty) == true
  }

  /*
   * this tests that isEmpty of a non-empty heap returns false
   */
  property("isEmpty2") = forAll { a: Int =>
    val h1 = insert(a, empty)
    isEmpty(h1) == false
  }

  /*
   * ****************** test deleteMin ********************
   */
  /*
   * this test that deleting min from an empty heap results in exception
   */
  property("deleteMin1") = forAll {
    a: Int =>
      try {
        val em = deleteMin(empty)
        false
      } catch {
        case e: NoSuchElementException => true
      }

  }

  /*
   * this test that deleting min from a heap with one element results in an empty heap
   */
  property("deleteMin2") = forAll {
    a: Int =>
      val h = insert(a, empty)
      val h2 = deleteMin(h)
      isEmpty(h2) == true
  }

  /*
   * this test that deleting min element from a heap with two elements x,y (x<y, inserted in order first x then y) gives a heap with min y
   */
  property("deleteMin3") = forAll {
    (a: Int, b: Int) =>
      /*      val less = if (a < b) a else if (b < a) b else a - 1
      val more = if (a < b) b else if (b < a) a else a*/
      val less = 4
      val more = 5
      val h1 = insert(less, empty)
      val h2 = insert(more, h1)
      val h3 = deleteMin(h2)
      findMin(h3) == more
  }

  /*
   * this test that deleting min element from a heap with two elements x,y (x>y, inserted in order first x then y) gives a heap with min x
   */
  property("deleteMin4") = forAll {
    (a: Int, b: Int) =>
      /*      val less = if (a < b) a else if (b < a) b else a
      val more = if (a < b) b else if (b < a) a else a + 1*/
      val less = 4
      val more = 5
      val h1 = insert(more, empty)
      val h2 = insert(less, h1)
      val h3 = deleteMin(h2)
      findMin(h3) == more
  }

  /*
   * this test that deleting min twice from a heap with one element result in exception
   */
  property("deleteMin5") = forAll {
    a: Int =>
      try {
        val h1 = insert(a, empty)
        val h2 = deleteMin(h1)
        val h3 = deleteMin(h2)
        false
      } catch {
        case e: NoSuchElementException => true
      }
  }

  /*
   * this test the functinality of inserting two of the same elements in a heap and then deleting the two elements one by one
   */
  property("inserttwoofsamevalue") = forAll {
    a: Int =>
      val h1 = insert(a, empty)
      val h2 = insert(a, h1)
      findMin(h2) == a
      val h3 = deleteMin(h2)
      findMin(h3) == a
      val h4 = deleteMin(h3)
      isEmpty(h4) == true
  }

  /*
   * ******************** test meld *********************
   */
  /*
   * this test that melding two empty heaps result in an empty heap
   */
  property("meldTest1") = forAll {
    a: Int =>
      val h = meld(empty, empty)
      isEmpty(h) == true
  }

  /*
   * this test that melting an empty heap with a non-empty heap result in a non-empty heap
   */
  property("meldTest2") = forAll {
    a: Int =>
      val h1 = insert(a, empty)
      val h2 = meld(h1, empty)
      isEmpty(h2) == false
  }

  /*
   * this test deleting one element from two heaps of size 1 and 0 result in an empty heap
   */
  property("meldTest3") = forAll {
    a: Int =>
      val h1 = insert(a, empty)
      val h2 = meld(h1, empty)
      val h3 = deleteMin(h2)
      isEmpty(h3) == true
  }

  /*
   * this test the validity of min of melding two heaps (size 0 and 1)
   */
  property("meldTest4") = forAll {
    a: Int =>
      val h1 = insert(a, empty)
      val h2 = meld(h1, empty)
      findMin(h2) == a
  }

  /*
   * this test the validity of min of melding two heaps of size 1 containing same element
   */
  property("meldTest5") = forAll {
    a: Int =>
      val h1 = insert(a, empty)
      val h2 = insert(a, empty)
      val h3 = meld(h1, h2)
      findMin(h3) == a
  }

  /*
   * this test the validity of min of melding two heaps of size 2 and 1
   */
  property("meldTest6") = forAll {
    a: Int =>
      val h1 = insert(1, empty)
      val h2 = insert(1, empty)
      val h3 = insert(2, h1)
      val h4 = meld(h1, h3)
      findMin(h4) == 1
  }

  /*
   * this test the find min functionality of melded heap with another element added to it
   */
  property("meldTest7") = forAll {
    a: Int =>
      val h1 = insert(2, empty)
      val h2 = insert(2, empty)
      val h3 = meld(h1, h2)
      val h4 = insert(1, h3)
      findMin(h4) == 1
  }

  /*
   * this test the deleteMin functionality of two heaps melded
   */
  property("meldTest8") = forAll {
    a: Int =>
      val h1 = insert(a, empty)
      val h2 = insert(a, empty)
      val h3 = meld(h1, h2)
      findMin(h3) == a
      val h4 = deleteMin(h3)
      findMin(h4) == a
      val h5 = deleteMin(h4)
      isEmpty(h5) == true
  }

  /*
   * test to meld two heaps with different elements and make sure the right element is still the smallest in the melded heap
   */
  property("meldTest9") = forAll {
    (a: Int) =>
      val h1 = insert(1, empty)
      val h2 = insert(2, empty)
      val h3 = meld(h1, h2)
      val h4 = meld(h2, h1)
      findMin(h3) == findMin(h4)
      findMin(h3) == 1
  }

  /*
   * test to meld two heaps containing same elements and then do delete and then add of the same element and then checking the size
   */
  property("meldTest10") = forAll {
    a: Int =>
      val h1 = insert(2, empty)
      val h2 = insert(2, empty)
      val h3 = meld(h1, h2)
      val h4 = meld(h2, h1)
      val h5 = deleteMin(h3)
      val h6 = deleteMin(h4)
      findMin(h5) == 2
      findMin(h6) == 2
      val h7 = insert(2, h5)
      val h8 = insert(2, h6)
      findMin(h7) == 2
      findMin(h8) == 2
      val h9 = deleteMin(deleteMin(h7))
      val h10 = deleteMin(deleteMin(h8))
      isEmpty(h9)
      isEmpty(h10)
  }

  /*
   * test to meld two heaps containing same elements and then do delete and then add of the same element and then checking the size
   * this is similar to 10 but with different values
   */
  property("meldTest10.5") = forAll {
    a: Int =>
      val h1 = insert(1, empty)
      val h2 = insert(2, empty)
      val h3 = meld(h1, h2)
      val h4 = meld(h2, h1)
      val h5 = deleteMin(h3)
      val h6 = deleteMin(h4)
      findMin(h5) == 2
      findMin(h6) == 2
      val h7 = insert(3, h5)
      val h8 = insert(3, h6)
      findMin(h7) == 2
      findMin(h8) == 2
      val h9 = deleteMin(deleteMin(h7))
      val h10 = deleteMin(deleteMin(h8))
      isEmpty(h9)
      isEmpty(h10)
  }

  /*
   * test to meld two heaps containing same elements and then do delete and then add of the same element and then checking the size
   * this is similar to 10 but with different values
   */
  property("meldTest10.6") = forAll {
    a: Int =>
      val h1 = insert(1, empty)
      val h2 = insert(2, empty)
      val h3 = meld(h1, h2)
      val h4 = meld(h2, h1)
      val h5 = deleteMin(h3)
      val h6 = deleteMin(h4)
      findMin(h5) == 2
      findMin(h6) == 2
      val h7 = insert(1, h5)
      val h8 = insert(1, h6)
      findMin(h7) == 1
      findMin(h8) == 1
      val h9 = deleteMin(deleteMin(h7))
      val h10 = deleteMin(deleteMin(h8))
      isEmpty(h9)
      isEmpty(h10)
  }

  /*
   * test to check that melding two empty heaps and then inserting results in non-empty
   */
  property("meld11") = forAll {
    a: Int =>
      val h1 = meld(empty, empty)
      val h2 = insert(a, h1)
      findMin(h2) == a
      isEmpty(h2) == false
  }

  /*
   * test to check the functionality of melding two melded heaps (
   */
  property("meld12") = forAll {
    a: Int =>
      val h1 = meld(insert(1, empty), insert(2, empty))
      val h2 = meld(insert(3, empty), insert(4, empty))
      val h3 = meld(h1, h2)
      findMin(h3) == 1
      isEmpty(h3) == false
      isEmpty(deleteMin(h3)) == false
      isEmpty(deleteMin(deleteMin(h3))) == false
      isEmpty(deleteMin(deleteMin(deleteMin(h3)))) == false
      isEmpty(deleteMin(deleteMin(deleteMin(deleteMin(h3)))))
  }

  /*
   * test to check the functionality of melding four heaps one by one
   */
  property("meld13") = forAll {
    a: Int =>
      val h1 = insert(1, empty)
      val h2 = meld(insert(2, empty), h1)
      val h3 = meld(insert(3, empty), h2)
      val h4 = meld(insert(4, empty), h3)
      findMin(h3) == 1
      isEmpty(h4) == false
      isEmpty(deleteMin(h4)) == false
      isEmpty(deleteMin(deleteMin(h4))) == false
      isEmpty(deleteMin(deleteMin(deleteMin(h4)))) == false
      isEmpty(deleteMin(deleteMin(deleteMin(deleteMin(h4)))))
  }

  /*
   * test to check the functionality of melding four heaps one by one (same as 13 but in different order)
   */
  property("meld14") = forAll {
    a: Int =>
      val h1 = insert(4, empty)
      val h2 = meld(insert(3, empty), h1)
      val h3 = meld(insert(2, empty), h2)
      val h4 = meld(insert(1, empty), h3)
      findMin(h3) == 2
      findMin(h4) == 1
      findMin(h2) == 3
      findMin(h1) == 4
      isEmpty(h4) == false
      isEmpty(deleteMin(h4)) == false
      isEmpty(deleteMin(deleteMin(h4))) == false
      isEmpty(deleteMin(deleteMin(deleteMin(h4)))) == false
      isEmpty(deleteMin(deleteMin(deleteMin(deleteMin(h4)))))
  }

  lazy val genHeap: Gen[H] = {
    for {
      elem <- arbitrary[Int]
      hp <- oneOf(empty, genHeap)
    } yield insert(elem, hp)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /*
   * let's map out the things that need to be tested:
   * the available methods:
   * empty -> the empty heap
   * isEmpty(h: H): Boolean -> to check if the given heap is empty
   * insert(x: Int, h: H): H -> to add the element x in the heap h
   * meld(h1: H, h2: H): H -> to merge the two heaps
   * findMin(h: H): Int -> to find the minimum of the heap h
   * deleteMin(h: H): H -> to delete the minimum element of the heap h
   * 
   * so the guess the things I should be testing are:
   * - what's the minimum of the empty heap?
   * - if I insert an element x to an empty heap then x should be the minimum of the resulting heap
   * - if I insert two elements x and y to an empty heap then the minimum of the resulting heap should be the min(x,y)
   * - if I insert two elements x, y (x<y) to an empty heap then delete the minimum of the the resulting heap then
   * the minimum of the heap should be y
   * - if I insert an element x to an empty heap then the minimum of the resulting heap should be x, afterwards if I
   * insert another element y (y<x) to the same heap then the minimum of the resulting heap should be y
   */

}
