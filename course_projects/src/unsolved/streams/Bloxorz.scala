package unsolved.streams

import unsolved.streams._
/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terraint to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1, 3)
    val goal = Pos(5, 8)
  }

  println(InfiniteLevel.solution)

  /**
   * A simple level constructed using the StringParserTerrain
   */
  abstract class Level extends Solver with StringParserTerrain

  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println(Level0.solution)

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends Level {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }
  println("===neighborsWithHistory")
  println(Level1.neighborsWithHistory(Level1.Block(Level1.Pos(1, 1), Level1.Pos(1, 1)), List(Level1.Left, Level1.Up)).take(2).toList)
  println("===newNeighborsOnly")
  println(Level1.newNeighborsOnly(
    Set(
      (Level1.Block(Level1.Pos(1, 2), Level1.Pos(1, 3)), List(Level1.Right, Level1.Left, Level1.Up)),
      (Level1.Block(Level1.Pos(2, 1), Level1.Pos(3, 1)), List(Level1.Down, Level1.Left, Level1.Up))
    ).toStream,

    Set(Level1.Block(Level1.Pos(1, 2), Level1.Pos(1, 3)), Level1.Block(Level1.Pos(1, 1), Level1.Pos(1, 1)))
  ).take(2).toList)

  println("===from test")

//  println(Level1.from(List((Level1.startBlock, List())).toStream, Set()).take(1).toList)
  println(Level1.pathsFromStart.take(15).toList)
  
  println(Level1.pathsToGoal.take(3).toList.mkString("\n"))
  println("===solution")
  println(Level1.solution)

  //println(Level1.solution)
}
