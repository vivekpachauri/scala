package solved.funsets



object Main extends App {
  import FunSets._;
  //  println(contains(singletonSet(1), 1))
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = singletonSet(4)
  val s5 = singletonSet(5)
  val s6 = singletonSet(6)
  val s7 = singletonSet(7)
  val s8 = singletonSet(8)
  val sa = union(s2, s4)
  val sb = union(sa, s6)
  val sc = union(sb, s8)
  assert(contains(sc, 2), "contains 2")
  assert(contains(sc, 4), "contains 4")
  assert(contains(sc, 6), "contains 6")
  assert(contains(sc, 8), "contains 8")
  /* create a filter that lets only even numbers pass */
  def evenFilter(x: Int): Boolean = if (x % 2 == 0) true else false
  /*  println("sc")
  printSet(sc)*/
  assert(forall(sc, evenFilter), "sc should only contains even elements")
  //  println(forall(sc, evenFilter))

  assert(contains(sc, 2), "contains 2")
  assert(contains(sc, 4), "contains 4")
  assert(contains(sc, 6), "contains 6")
  assert(contains(sc, 8), "contains 8")
  /* create a filter that lets only even numbers pass */
  def oddFilter(x: Int): Boolean = if (x % 2 != 0) true else false
  assert(!exists(sc, oddFilter), "sc should not contains any odd element")
  val sd = union(sc, s5)
  /*  println("sd")
  printSet(sd)*/
  assert(exists(sd, oddFilter), "sd should pass because it now contains 5")

}
