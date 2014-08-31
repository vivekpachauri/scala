package solved.objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

}

abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   *  in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = filter0(p, new Empty)
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet = {
    /* base case - if this is empty set then simply return  that, doesn't matter at this point if that is empty or not */
    if (this.isEmpty) that
    /* if this is not empty then check if the head of this is included in that */
    else if (that.contains(this.head)) {
      /* if yes then we don't want to add it again, simply recurse of the rest of this */
      this.tail.union(that)
    } /* else if no then we need to add this to that and then recurse on the tail */ else {
      this.tail.union(that.incl(this.head))
    }
  }
  // Hint: the method "remove" on TweetSet will be very useful.
  /*
   * the intended logic is that you start witn an empty trending and then you continuously iterate on the
   * existing tweets in the tweet set and as you remove a tweet from tweet set, you add it to the trending object
   */
  /* 
   * I think I can implement this in the abstract class, I don't need to provide explicit implementation in the two
   * extending sub-classes
   */
  /* 
   * however, I need to create the initial Trending object, and because of that I am thinking of creating
   * a helper function
   */

  def ascendingByRetweet: Trending = ascendingByRetweetRec(this, new EmptyTrending)

  /*
   * pseudo-code -
   * - base case - if the param set is empty then return the accumulator
   * else
   * - find the smallest tweet in the param
   * - add the smallest tweet to trending
   * - remove the smallest tweet from this set
   * - recurse on the resulting set and the generated accu
   */  
  def ascendingByRetweetRec(param: TweetSet, accu: Trending ):Trending = {
    if ( param.isEmpty ) accu
    else {
      ascendingByRetweetRec(param.remove(param.findMin), accu + param.findMin)
    }
  }

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
  // -------------------------------------------------------------------------
}

class Empty extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = accu

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
  // -------------------------------------------------------------------------
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {

    /* 
     * lesson learned - keep in mind the limitations and differences when working with immutable objects
     * as compared to mutable objects
     */
    if (p(elem))     this.left.filter0(p, right.filter0(p, accu.incl(elem)))
    
    else this.left.filter0(p, this.right.filter0(p, accu))
  }

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------
}

/**
 * This class provides a linear sequence of tweets.
 */
abstract class Trending {
  def +(tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def +(tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  /**
   * Appends tw to the end of this sequence.
   */
  def +(tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val allTweets = TweetReader.allTweets
  
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")

  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")


  /*
   * the filter is supposed to check if the text in the tweet contains a word from the google list
   * 
   */
  def googleFilter(tweet: Tweet): Boolean = {
    google.exists((x:String)=> if ( tweet.text.contains(x)) true else false)
  }
  
  def appleFilter(tweet:Tweet): Boolean = {
    apple.exists( (x:String) => if ( tweet.text.contains(x) ) true else false )
  }

    /*
   * take all the tweets and apply a filter f to them and capture the returned set
   * let's worry later about the logic of the filter
   */
  val googleTweets: TweetSet = allTweets.filter(googleFilter)

  val appleTweets: TweetSet = allTweets.filter(appleFilter)

  // Q: from both sets, what is the tweet with highest #retweets?
  val trending: Trending = appleTweets union googleTweets ascendingByRetweet
}

object Main extends App {
  // Some help printing the results:
  
   println("RANKED:")
//   GoogleVsApple.googleTweets foreach println
   GoogleVsApple.trending foreach println
}
