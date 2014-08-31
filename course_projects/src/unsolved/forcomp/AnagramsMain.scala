package unsolved.forcomp

object AnagramsMain extends App {
  import Anagrams._;

  //assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  wordOccurrences("abcd").map(x=>println(x))
}