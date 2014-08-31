package solved.forcomp

import common._

object Anagrams {

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
    val lowerCaseWord = w.toLowerCase()
    val asList = lowerCaseWord.toList
    val sortedList = asList.sortWith((one, two) => one < two)
    val map = sortedList.groupBy(c => c)

    val newMap = map transform ((k, v) => v.length)
    newMap.toList.sortWith((one, two) => one._1 < two._1)
  }

  def wordOccurrencesUsingPack(w: Word): Occurrences = {
    /* let's use the pack function created as exercise, replacing span with partition should produce the desired result */
    def pack[T](list: List[T]): List[List[T]] = list match {
      case Nil => Nil
      case list => {
        val partition = list.partition(arg => arg == list.head)
        partition._1 :: pack(partition._2)
      }
    }

    val lowerCaseWord = w.toLowerCase()
    val asList = lowerCaseWord.toList
    val sortedList = asList.sortWith((one, two) => one < two)
    pack(sortedList).map(a => (a.head, a.size))
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val appended = s.foldRight("")((a, b) => a + b)
    wordOccurrences(appended)
  }

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
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy((e: Word) => wordOccurrences(e))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).toList.flatten

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
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    def combinationsHelper(list: List[(Char, Int)]): List[List[(Char, Int)]] = {

      def individual(elem: (Char, Int)): List[(Char, Int)] = {
        if (elem._2 == 1) List(elem)
        else List(elem) ::: individual((elem._1, elem._2 - 1))
      }

      list match {
        case Nil              => List(List())
        case (char, 1) :: Nil => List(List((char, 1)))
        case (char, n) :: Nil => List(List((char, n))) ::: combinationsHelper(List((char, n - 1)))
        case (char, int) :: ls => {
          val first = combinationsHelper(List((char, int)))
          val second = individual(char, int) map (x => combinationsHelper(ls) map (y => x :: y))
          val third = combinationsHelper(ls)
          first ::: second.flatten ::: third
        }
      }
    }
    occurrences match {
      case Nil => List(List())
      case a   => List(List()) ::: combinationsHelper(a)
    }
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
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subtractOne(list: List[(Char, Int)], toRemove: (Char, Int)): List[(Char, Int)] = {
      val withZero = for {
        elem <- list
      } yield if (elem._1 != toRemove._1) elem else (elem._1, elem._2 - toRemove._2)
      withZero filterNot (elem => elem._2 == 0)
    }

    y match {
      case Nil          => x
      case elem :: Nil  => subtractOne(x, elem)
      case elem :: tail => subtract(subtractOne(x, elem), tail)
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
    /* all right now its just you and me, and you are going down...no one is going to save you */
    /* - generate the occurrence for the sentence */
    val sentenceOccurrence = this.sentenceOccurrences(sentence)
    /* - generate the combination for the sentence anagram */
    val occurenceCombinations = this.combinations(sentenceOccurrence)
    /* iterate over each occurrence in the combination 
     *  for each occurrence in the combination find the mapped words 
     *  
     *  -- let's say it is a-2, b-2,
     *  so the combination is going to be 
     *  Nil,
     *  a-1
     *  a-2
     *  a-1, b-1
     *  a-1, b-2
     *  a-2, b-1
     *  a-2, b-2
     *  b-1
     *  b-2
     *  
     *  so to generate the sentence anagram, first iterate over all the 
     *  */

    def helperGetSentenceByOccurence(occur: Occurrences): Sentence = {
      this.dictionaryByOccurrences.get(occur).toList.flatten
    }

    def helper(sentenceOcc: Occurrences): List[Sentence] = {
      val ocs = this.combinations(sentenceOcc)
      ocs match {
        case Nil          => List(List())
        case head :: tail => helperGetSentenceByOccurence(head) :: helper(subtract(sentenceOcc, head))
      }
    }
    helper(sentenceOccurrence)
  }

}

object Main {
  def main(arg: Array[String]) = {
    //    val l = "abcdsksbabdjs".toList
    //   System.out.println(l.partition(x => x == l.head))
    //    nine
    //    println((NewAnagrams.dictionaryByOccurrences take 100) filter ((a) => a._2.length > 1) mkString "\n")
    println(NewAnagrams.wordAnagrams("parental"))

  }

  def one() = {
    System.out.println(Anagrams.wordOccurrences("absbsjajsjshdbsbansndbdjsh"))
    System.out.println(Anagrams.wordOccurrencesUsingPack("absbsjajsjshdbsbansndbdjsh"))
  }

  def two() = {
    System.out.println(Anagrams.sentenceOccurrences(List("one", "two", "three", "four")))
  }

  def three() = {
    //        	System.out.println(Anagrams.dictionary)
    //    System.out.println(Anagrams.dictionary.map(a => System.out.println(a)))
    Anagrams.dictionary.foreach(a => System.out.println(a))
  }

  def four() = {
    Anagrams.dictionaryByOccurrences.foreach(a => System.out.println(a))
  }

  def five() = {
    System.out.println(Anagrams.wordAnagrams("cat"))
  }

  def six() = {
    val list = List(('a', 2))

    /*    def combinations(list: List[(Char, Int)]): List[List[(Char, Int)]] = list match {
      case Nil => List(List())
      case (c,  i) :: ls => {
        if ( i == 1 ) List((c,i)) :: combinations(ls)
        else List(individual((c,i))) ::: combinations((c, i-1) :: ls)
      }
    }*/

    def combinations2(list: List[(Char, Int)]): List[List[(Char, Int)]] = {

      def innerCombinationsOfOneCharacter(list: List[(Char, Int)]): List[List[(Char, Int)]] = list match {
        case Nil => List(List())
        case a :: Nil => for {
          items <- individual(a)
        } yield List(items)
      }

      def innerCombinationsOfOneCharacterUpdated(comb: (Char, Int)): List[List[(Char, Int)]] = {
        for {
          items <- individual(comb)
        } yield List(items)
      }

      def innerCombinationsOfTwoCharacters(first: (Char, Int), second: (Char, Int)): List[List[(Char, Int)]] = {
        for {
          items <- individual(first)
          items2 <- individual(second)
        } yield List(items) ::: List(items2)
      }

      val twoCombinations = for {
        items <- list
        items2 <- list
        if (items._1 < items2._1)
      } yield innerCombinationsOfTwoCharacters(items, items2)

      val oneCombinations = for {
        items <- list
      } yield innerCombinationsOfOneCharacterUpdated(items)
      //      innerCombinationsOfTwoCharacters(list.head, list.tail.head)
      //      innerCombinationsOfOneCharacterUpdated(list.head)
      oneCombinations.flatten

      //      twoCombinations.flatten

      list match {
        case Nil              => List(List())
        case (char, 1) :: Nil => List(List((char, 1)))
        case (char, n) :: Nil => List(List((char, n))) ::: combinations2(List((char, n - 1)))
        //        case (char, int) :: ls => combinations(List((char, int))) ::: ( List((char, int)) :: combinations(ls) ) ::: combinations(ls)
        //        case (char, int) :: ls => combinations(List((char, int))) ::: (combinations(ls) map (x => (char, int) :: x)) ::: combinations(ls)
        case (char, int) :: ls => {
          val first = combinations2(List((char, int)))
          //          val second = (combinations(ls) map (x => (char, int) :: x))
          val second = individual(char, int) map (x => combinations2(ls) map (y => x :: y))
          val third = combinations2(ls)
          first ::: second.flatten ::: third
        }
      }
    }

    /*    def comb(list: List[(Char, Int)]): List[List[(Char, Int)]] = {
      
    }*/

    def individual(elem: (Char, Int)): List[(Char, Int)] = {
      if (elem._2 == 1) List(elem)
      else List(elem) ::: individual((elem._1, elem._2 - 1))
    }
    //    System.out.println(individual(('a', 2)))
    System.out.println(Anagrams.combinations(List(('a', 2), ('b', 2), ('c', 2))))
    //        System.out.println(combinations2(List()))
    //    System.out.println(Anagrams.combinations(List()))
  }

  def seven() = {

    def subtractOne(list: List[(Char, Int)], toRemove: (Char, Int)): List[(Char, Int)] = {
      val withZero = for {
        elem <- list
      } yield if (elem._1 != toRemove._1) elem else (elem._1, elem._2 - toRemove._2)
      withZero filterNot (elem => elem._2 == 0)
    }

    def subtract(list: List[(Char, Int)], toRemove: List[(Char, Int)]): List[(Char, Int)] = toRemove match {
      case Nil          => list
      case elem :: Nil  => subtractOne(list, elem)
      case elem :: tail => subtract(subtractOne(list, elem), tail)
    }

    val first = List(('i', 1), ('j', 1), ('m', 2), ('y', 1))
    val second = List(('m', 1), ('y', 1))
    System.out.println("here")
    //    System.out.println(Anagrams.subtract(first, second))
    System.out.println(Anagrams.subtract(first, second))
  }

  def eigth() = {
    System.out.println(Anagrams.sentenceAnagrams(List("i", "love", "you")))
  }

  def nine() = {
    System.out.println(Anagrams.wordOccurrences("you"))
    val occurrence = Anagrams.sentenceOccurrences(List("you", "olive"))
    System.out.println(occurrence)
    val combination = Anagrams.combinations(occurrence)
    System.out.println(combination)
    //    System.out.println(Anagrams.dictionaryByOccurrences.get(Anagrams.sentenceOccurrences(List("olive"))))
  }
}

