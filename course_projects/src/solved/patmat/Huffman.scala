package solved.patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Fork(l, r, c, w) => weight(l) + weight(r)
    case Leaf(char, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(l, r, c, w) => chars(l) ::: chars(r)
    case Leaf(c, w) => ::(c, Nil)
  }

  def main(args: Array[String]) = {
    System.out.println(makeCodeTree(new Leaf('a', 2), new Leaf('b', 3)))
    System.out.println(chars(new Fork(new Leaf('a', 2), new Leaf('b', 3), List.empty[Char], 2)))
    System.out.println(times(string2Chars("aba")))
    val list = makeOrderedLeafList(times(string2Chars("aabbaaaccdc")))
    System.out.println(list)
    System.out.println(combine(list))
    System.out.println(combine(combine(list)))
    System.out.println("until: " + until(singleton, combine)(list))
    System.out.println(this.createCodeTree(string2Chars("aabbaaaccdc")))
    System.out.println("secret: " + decodedSecret)
    System.out.println(secret)
    System.out.println(encode(frenchCode)(string2Chars("huffmanestcool")))
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    /*
     * to do this what I am thinking of implementing is a recursive method
     */

    /* 
     * I need to define one more method which takes a list of pair and a character and updates the list of pair according to the param character
     */

    def updatePartList(char: Char, list: List[(Char, Int)]): List[(Char, Int)] = {
      if (list.isEmpty) ::((char, 1), Nil)
      else if (list.head._1 == char) ::((list.head._1, list.head._2 + 1), list.tail)
      else ::(list.head, updatePartList(char, list.tail))
    }

    def timesHelper(chars: List[Char], aggregate: List[(Char, Int)]): List[(Char, Int)] = {
      if (chars.isEmpty) aggregate
      //      else if ( aggregate.isEmpty ) timesHelper(chars.tail, :: ((chars.head, 1), aggregate)
      else timesHelper(chars.tail, updatePartList(chars.head, aggregate))
    }

    timesHelper(chars, Nil)
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def getMaxPair(list: List[(Char, Int)]): (Char, Int) = {
      def getMaxPair(list: List[(Char, Int)], minSoFar: (Char, Int)): (Char, Int) = {
        if (list.isEmpty) minSoFar
        else if (list.head._2 > minSoFar._2) getMaxPair(list.tail, list.head)
        else getMaxPair(list.tail, minSoFar)
      }
      getMaxPair(list.tail, list.head)
    }

    def makeOrderedLeafListHelper(freqs: List[(Char, Int)], aggregator: List[Leaf]): List[Leaf] = {
      if (freqs.isEmpty) aggregator
      else {
        val max = getMaxPair(freqs)
        System.out.println("minimum:  " + max._1 + "  " + max._2)
        /*
        if freqs is not empty then i need to retrieve the maximum value form the list,
        update the aggregator list with a new leaf created using this maximum value,
        recursively call the helper method with the new aggregator and the new freqs created after
        deleting the current maximum from the list
        */
        makeOrderedLeafListHelper(freqs.filterNot((arg: (Char, Int)) => (arg._1 == max._1 && arg._2 == max._2)), ::(new Leaf(max._1, max._2), aggregator))
      }
    }
    if (freqs.isEmpty) Nil
    else makeOrderedLeafListHelper(freqs, Nil)
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    if ( trees.isEmpty ) false
    else trees.tail.isEmpty
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    /*
     * this is not that complicated either, creating a Fork node out of two Leaf nodes are easy, I have makeCodeTree method to create
     * the Fork node, after that I have to use some helper method to generate a new list of code trees which is in sorted order by weight
     */
    /*
	   * first trivial case, if the site of 'trees' is less than 2 then simply return it
	   */
    def combineHelper(newNode: CodeTree, trees: List[CodeTree]): List[CodeTree] = {
      if (trees.isEmpty) ::(newNode, Nil)
      else if (weight(newNode) < weight(trees.head)) /* then append the new node to the beginning of the tree */ ::(newNode, trees)
      else /* else append the first element of the tree to the result of calling the method recursively on the tail */ ::(trees.head, combineHelper(newNode, trees.tail))
    }
    if (trees.isEmpty || trees.tail.isEmpty) trees
    else {
      /*       retrieve the first and second elements of the list */
      val first = trees.head
      val second = trees.drop(1).head
      val newList = trees.drop(2)

      /*       generate a Fork out of these */
      val newNode = makeCodeTree(first, second)
      combineHelper(newNode, newList)
    }
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(singletonCheck: (List[CodeTree] => Boolean), combineMethod: (List[CodeTree] => List[CodeTree]))(trees: List[CodeTree]): CodeTree = {
/*    var tree = trees
    while (!(singletonCheck(tree))) {
      tree = combineMethod(tree)
    }
    tree.head*/
    if ( singletonCheck(trees) ) trees.head
    else until(singletonCheck, combineMethod)(combineMethod(trees))
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeHelper(currentTree: CodeTree, root: CodeTree, bits: List[Bit], aggregate: List[Char]): List[Char] = currentTree match {
      case Leaf(c, w) => decodeHelper(root, root, bits, aggregate ::: ::(c, Nil))
      case Fork(l, r, list, weight) =>  if (bits.isEmpty) aggregate else if (bits.head == 0) decodeHelper(l, root, bits.tail, aggregate) else decodeHelper(r, root, bits.tail, aggregate)
    }
    /*    {
    	
    	 * if no more bits then return the aggregate
    	 
      if ( bits.isEmpty ) aggregate
      
       * else if you are currently at leaf node then add the character of the leaf node to the end of the aggregate and start from the root
       
      else
        
         * else read the next bit, if it is zero then go down the left sub-tree, 
         
        aggregate
    }*/

    decodeHelper(tree, tree, bits, Nil)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  /* yes I can, it says huffmanest cool */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeHelper(currentNode: CodeTree, root: CodeTree, text: List[Char], aggregate: List[Bit]):List[Bit] = currentNode match {
      case Leaf(c, w) => encodeHelper(root, root, text.tail, aggregate)
      /*
       * if at leaf then remove the character from the text, restart decoding from root again for the remaining characters
       */
      case Fork(l, r, list, weight) => if ( text.isEmpty ) aggregate else if ( chars(l).contains(text.head) ) encodeHelper(l, root, text, aggregate ::: :: (0, Nil)) else encodeHelper(r, root, text, aggregate ::: ::(1, Nil))
      /*
       * if at fork and no more characters left in text then done
       */
        /*
         * else if left sub tree contains the next character in text then recurse of left tree and append a 0 to the end of aggregate, else recurse on right and append a 1 at the end of aggregate
         */
    }
    encodeHelper(tree, tree, text, Nil)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    if (table.isEmpty ) Nil
    else if ( table.head._1 == char ) table.head._2
    else codeBits(table.tail)(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    def convertHelper(current: CodeTree, root: CodeTree): CodeTable = current match {
      case Leaf(c, w) => :: ( (c, encode(root)( :: (c, Nil))), Nil)
      case Fork(l, r, list, weight) => convertHelper(l, root) ::: convertHelper(r, root)
    }
    convertHelper(tree, tree)
  }
  /*
   * so i have to convert a code tree into a code table,
   * for that i have to iterate over all the 
   */
  /*
   * base case - if the currnet tree is a leaf then call encode to the aggregate list 
   */

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    if (a.isEmpty ) b
    else :: (a.head, mergeCodeTables(a.tail, b))
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)
    
    def quickEncodeHelper(text: List[Char], aggregate: List[Bit]): List[Bit] = {
      if (text.isEmpty ) aggregate
      else quickEncodeHelper(text.tail, aggregate ::: codeBits(table)(text.head) )
    }
    /*
     * now using this table keep appending the value bit list to get the result
     */
    quickEncodeHelper(text, Nil)
  }
}
