package solved.forcomp

import common._

object NewAnagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  def printSentence(s: Sentence) = s mkString (" ")

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {

    def abc(d: String): List[(Char, Int)] =
      {
        if (d == Nil) Nil
        if (d.equals("")) Nil
        else {
          val (first, second) = d partition (x => x == d.head)
          (d.head, first length) :: abc(second)
        }
      }
    abc(w.toLowerCase.toList.sorted.mkString)
  }

  //convert to lower case, sort, listify, use filterall to extract desired data

  //  def wordOccurrencesUsingPack(w: Word): Occurrences = ???

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s mkString)

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences
  //???

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occ: Occurrences): List[Occurrences] = {
    if (occ.length == 0)
      List(List())
    val lst = for {
      (a, b) <- occ if (b > 0)
    } yield {
      val first = occ
      val second = occ filterNot (x => (x._1 == a) && (x._2 == b))
      val third = (a, (b - 1))
      first :: combinations(third :: second)

    }
    val toRet = lst.toSet.toList flatMap ((x: List[Occurrences]) => x)
    val filteredRet = toRet map (x => x.filterNot(y => y._2 == 0))
    val sortedRet = filteredRet map (x => x.sortWith((a, b) => a._1 < b._1))
    sortedRet.toSet.toList
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = { /* we can go one element at a time meaning for a pat matching on y's elements and for each elem find its counter part in x
	   * , update its count and build upon the list that way
	   */
    y match {
      case Nil => x
      case fst :: Nil => {
        val (hit, miss) = x partition (xx => xx._1 == fst._1)
        if (hit.length == 0) throw new Exception("no matching " + fst._1 + " found in x")
        else {
          val elemX = hit.head
          if (elemX._2 - fst._2 > 0) {
            (elemX._1, (elemX._2 - fst._2)) :: miss
          }
          else miss
        }
      }
      case fst :: rst => {
        val (hit, miss) = x partition (xx => xx._1 == fst._1)
        if (hit.length == 0) throw new Exception("no matching " + fst._1 + " found in x")
        else {
          val elemX = hit.head
          if (elemX._2 - fst._2 > 0) {
            (elemX._1, (elemX._2 - fst._2)) :: subtract(miss, rst)
          }
          else {
            subtract(miss, rst)
          }
        }
      }
    }
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    /* time to start thinking about the last method
     * we now have all the required pieces,
     * -we have a way to take a word and break it into its occurrence list which tells us which characters are in that word
     * and how many times
     * - we have a way to give us the occurrence list for the entire sentences
     * - we have a way to get all the words that have the same occurrence list therefore this way we can take a word,
     * generate its occurrence list and then find which other words have that same occurrence list which in turn tells
     * us the anagrams for that word
     * - we have a way of taking an occurrence list and generate all the possible combinations of the various occurrences
     * of that word so that we can identify what are the possible word combination that could be made with the number of
     * characters present in that word
     * - we have a way of subtracting two occurrence lists so that we can take a word, generate its occurrence list combinations
     * and then for each occurrence list in that combination we can find the possible words that match that occurrence and then
     * subtract the occurrence of the matched word from the original occurrence list and repeat this until we have exhausted
     * the occurrence list completely to generate a full match
     */
    /*
     * one main loop i definitely see is generating the occurrence combination of the sentence and for each occurrence in the combination
     * I have to check if there is a word that match this occurrence and if so then subtract this occurrence from the main occurrence
     * and then repeat the process
     */
    /*
     * need to figure out how to establish the termination condition and use the fact that unconsumed occurrance list will lead to
     * dropping all the words that exists so far according to the choices of the occurrances chosen so far,
     * perhaps the driver can take the existing choices of the occurrances as well along with the original occurrance
     * this way we can determine if the choices taken exhaust the original occurrance while at the same time clearly indicating
     * which words to be used as part of the sentence
     */
    /*
     * another train of though that we could pursue is if we should seperate the work of choosing the occurrance list combinations
     * which exhaust the original sentence occurrance list from the work of identifying the words that corresponds to those chosen
     * occurrances, meaning first we identify which combinations of occurrances exhaust the original occurrance and once we have the
     * list of such occurrances then we iterate over those occurrances and generate the sentences from those
     */
    def driver(occ: Occurrences): List[Sentence] = ???
    
    driver(sentenceOccurrences(sentence))
  }
}


