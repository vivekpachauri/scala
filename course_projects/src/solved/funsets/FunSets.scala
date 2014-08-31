package solved.funsets

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
  def singletonSet(elem: Int): Set = (_==elem)
    //(x:Int)=>if (x == elem ) true else false
/*  {
    def toReturn(x:Int):Boolean = {if (elem == x) true else false}
    toReturn
  }*/

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x:Int) => if (s(x) || t(x)) true else false

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = (x:Int) => if (s(x) && t(x)) true else false

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x:Int) => if (s(x) && (!t(x) ) ) true else false

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x:Int) => if ( s(x) && p(x) ) true else false

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
    def iter(a:Int): Boolean = {
      if ( a > 1000 ) false
      else if ( (s(a) == true) && (p(a) == true) ) true
      else iter(a+1)
    }
    iter(-1000)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def toReturn(x:Int):Boolean = {
      /* should return true if there exists in s an element e such that f(e) = x */
      def testFunc(y:Int):Boolean = {
        if (f(y) == x ) true else false
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
