package solved.recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    println("printing tests")
    println(pascal(0,2))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    /*
     * give that the numbers at the edge of triangles are 1 so we can use the following as base
     * case:
     */
    /*
     * the very first element of each level is 1 => if n is 0 then return 1
     */
    /*
     * the very last element of each level is 1 => if m is equal to n then return 1
     */
    /*
     * the very top level element could be 1 using either of the two logic
     */
    /*
     * if the element is not the first or the last element of the level then it is the
     * sum of the element at row r-1 column c-1 and the element at row r-1 and column c  
     */
    
    /* dammiiiiiitttttt */
    /* looks like I forgot add and else and wrote the second check asif instead of else if
     * fffffffffffffffffffffffffff
     */
/*    if ( c == 0 ) 1
    else if ( r == 0 ) 1
    else if ( c == r ) 1
    else return pascal(c, r-1) + pascal(c-1, r-1)*/
    
    if ( c == 0 ) 1
    else {
      if (r == 0 ) 1
      else
      {
        if ( r == c ) 1
        else
        {
          pascal (c - 1, r - 1) + pascal(c, r-1)
        }
      }
        
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    /*
     * here is the logic:
     * create an inner function that takes the parameter string and an integer value, the integer
     * value could represent how many open parentheses have been seen so far.
     */
    def innerBalance(chars: List[Char], numOpen: Int): Boolean = {
      /*
       * base case 1 - numOpen less than 0 means seeing a close parentheses without its matching
       * open parentheses and thus an unbalanced set of parentheses
       */
      if ( numOpen < 0 ) false;
      /*
       * base case 2 - empty character list with numOpen equal to 0 means balanced parentheses
       */
      else if ( (chars.isEmpty) && (numOpen == 0) ) true;
      /*
       * base case 3 - empty character list with numOpen not equal to 0 means unbalanced set of
       * parentheses
       */
      else if ( (chars.isEmpty) && (numOpen != 0) ) false;
      /*
       * recursive case 1 - if first of chars is '(' then call innerBalance with
       * rest of chars and numOpen = numOpen + 1
       */
      else if ( chars.head == '(' ) innerBalance(chars.tail, (numOpen+1));
      /*
       * recursive case 2 - if first of chars is ')' then call innerBalance with rest of
       * chars and numOpen = numOpen - 1
       */
      else if ( chars.head == ')' ) innerBalance(chars.tail, (numOpen -1 ));
      /*
       * recursive case 3 - if first of chars is neither ')' nor '(' then call innerBalance with
       * rest chars and numOpen
       */
      else innerBalance(chars.tail, numOpen);
    }
    innerBalance(chars, 0);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
	/*
	 * doesn't look like there is any way around unsing a helper function
	 * inside the countChange function
	 */
    /*
     * the multiplier is to represent minimum how many times the very
     * first coin in the list should be used in the combination being
     * tested
     */
    def helper(money: Int, coins: List[Int], multiplier: Int): Int = {
      /*
       * the first base case is that if you have to count 0 coins then
       * you can count it with 0 coins of each type or without using any
       * coin thus there is 1 way to count if the total is 0
       */
      if ( money == 0 ) 1
      /*
       * second base case is that if the total to count is less than
       * 0 then we cannot count it no matter what combination of the
       * coins we use
       */
      else if ( money < 0 ) 0
      /*
       * third base case is that if the total is not 0 and we don't
       * have any coins to use (coins list is empty)  then we 
       * cannot count the money
       */
      else if ( coins.isEmpty == true ) 0 
      /*
       * missed one more base case, if using multiplier number of 
       * coins of the first type will cause the total amount of 
       * money in the partial solution to be greater than the 
       * total money that we have to count up to then we cannot 
       * count using this combinations of coins starting with
       * multiplier number of the first coin types thus return 0
       */
      else if ( (coins.head * multiplier) > money ) 0
      /*
       * at this point the total is not 0 and the list is not empty
       * so need to make a recursive call at this point
       */
      /*
       * two recursive call should be made, one to shorten the list
       * of coins and the second will increase the number of first
       * coin type
       */
      else (countChange( (money - (coins.head * multiplier)), coins.tail) + helper(money, coins, multiplier + 1))
    }
    /*
     * start the countChange method by calling helper function with
     * the initial value of the multiplier being 0
     */
    helper(money, coins, 0)
  }
}
