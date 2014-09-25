package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x:Int)=>(x == elem )
  /*{
    def toReturn(x:Int):Boolean = {if (elem == x) true else false}
    toReturn
  }*/

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x:Int) => (s(x) || t(x))

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = (x:Int) => (s(x) && t(x))

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x:Int) => (s(x) && (!t(x) ) )

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x:Int) => ( s(x) && p(x) )

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      /* base case 1 - if we successfully iterated over all the range of all the intergers then return true */
      if (a > 1000) true
      /* base case 2 - if the current integer is part of the set but not valid according to predicate then return false */
      else if ((s(a) == true) && (p(a)==false) ) false
      /* recursive case, test the next integer one greater then a */
      else iter(a+1)
    }
    iter(-1000)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    /*def iter(a:Int): Boolean = {
      if ( a > 1000 ) false
      else if ( (s(a) == true) && (p(a) == true) ) true
      else iter(a+1)
    }
    iter(-1000)*/
    /* if this exists is to be implemented in terms of forall then the iterator doesn't need to be reimplemented
	the forall call will take care of the iteration, only the predicate to be tested is to be implemented in
	such a way that as soon as it pass it 
	Now to think again about what this exists function means:
	for any element inside the set, does that element satisfy the predicate, if yes then yes else no
	so the set that i use with forall could be a filter of the actual set and the predicate, this way
	if there is any element inside that satisfy s and p would be checked in the predicate to forall.
	The predicate sent to forall could be either s or p however the corner case of empty set s fails
	this test. The filter is not helping, can't get around the empty set base case.
	Think about modifying the predicate with a simple negation, this way if there is any element in s
	for which the original predicate pass, the modified predicate will fail and thus the forall will
	return immediately with false, if no element in s pass the modified predicate or if no element in s
	exist then forall will return true which will mean the exists fails
    */
	
    //forall(p,s)
    def invertedPredicate(x: Int): Boolean = !p(x)
    !forall(s, invertedPredicate)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def toReturn(x:Int):Boolean = {
      /* should return true if there exists in s an element e such that f(e) = x */
      def testFunc(y:Int):Boolean = {
        (f(y) == x )
      }
//      exists(s, y=>if (f(y) == x) true else false)
      exists(s, testFunc)
    }
    toReturn
  }
  /* let's analyze what the hell i did in the previous function */
  /*
   * let's say s contains 2 and 3, which means that if s is called with 2 or 3 it returns true else false,
   * now if I use map on s with a function x => x + 1
   * then it means that s now contains 3 and 4, this means that the new function i return should return true for
   * arguments 3 and 4 and false otherwise,
   * what i am returning is a function which calls f on its argument and then 
   */

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
