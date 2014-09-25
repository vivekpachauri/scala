package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = initializePopulation // List() // to complete: construct list of persons
  println("printing persons")
  persons.map(println(_))
  /* initialize the persons list while making sure that 1% of the population is infected
   * during beginning
   */

  /* shoud the aux method generate one person at a time or should it generate all 300 and then
   * randomly change 1 percent of these to be sick?? the second sounds better
   */
  private def initializePopulation(): List[Person] = {
    //    println("initializing the population")
    var persons = (1 to population).map((x: Int) => new Person(x))
    /* persons initialized with the required number of people, now change 1% to be infected */
    val onePercent = (population * 0.01).toInt
    var infectedIndex: List[Int] = Nil // list to capture the index of the person(s) that will be marked infected
    println("setting 1 percent to infected")
    while (infectedIndex.size < onePercent) {
      //generate the random index
      val index = randomBelow(persons.size)
      //      println(s"setting person at index $index as infected")
      //if this index doesn't already exist in the infectedIndex
      if (infectedIndex.contains(index) == false) {
        //then add it
        infectedIndex = index :: infectedIndex
        //and set the person at this index to be infected
        persons(index).infected = true
      }
    }
    persons.toList
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    override def toString = s"$id $infected $sick"

    //
    // to complete with simulation logic
    //
  }
}
