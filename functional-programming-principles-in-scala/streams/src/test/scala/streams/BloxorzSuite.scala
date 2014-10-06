package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait TestLevel extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """-----
        |-ooo-
        |-oST-
    	|-ooo-
        |-----""".stripMargin
  }  
  
  test("TestLevel and legalNeighbors") {
    new TestLevel {
    	assert(Block(Pos(2, 2), Pos(2, 2)).legalNeighbors === List())
    }
  }
  
  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(terrain(Pos(1, 1)), "1,1")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(!terrain(Pos(12, 14)), "12,14")
      assert(terrain(Pos(4, 7)), "4,7")
      assert(terrain(Pos(1, 4)), "1,4")
      assert(terrain(Pos(5, 6)), "5,6")
      assert(!terrain(Pos(-4, -5)), "-4,-5")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
      assert(goal == Pos(4, 7))
    }
  }

  test("neighborsWithHistory test case") {
    new Level1 {
      assert(terrain(Pos(1, 1)), "1,1")
      assert(Block(Pos(1, 1), Pos(1, 1)).legalNeighbors == List((Block(Pos(1, 2), Pos(1, 3)), Right), (Block(Pos(2, 1), Pos(3, 1)), Down)))
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet == Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)), (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("newNeighborsOnly test case") {
    new Level1 {
      assert(newNeighborsOnly(Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)), (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream, Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))).toSet == Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("Block: isStanding") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).isStanding)
      assert(!(Block(Pos(0, 1), Pos(0, 2)).isStanding))
    }
  }

  test("Block: isLegal") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).isLegal)
      assert(!(Block(Pos(0, 2), Pos(0, 3)).isLegal))
    }
  }

  test("Block: neighbors") {
    new Level1 {
      assert(Block(Pos(2, 2), Pos(2, 2)).neighbors.toSet == List(
        (Block(Pos(0, 2), Pos(1, 2)), Up),
        (Block(Pos(2, 0), Pos(2, 1)), Left),
        (Block(Pos(2, 3), Pos(2, 4)), Right),
        (Block(Pos(3, 2), Pos(4, 2)), Down)).toSet)
    }
  }

  test("Block: legalNeighbors") {
    new Level1 {
      assert(Block(Pos(2, 2), Pos(2, 2)).legalNeighbors.toSet == List(
        (Block(Pos(0, 2), Pos(1, 2)), Up),
        (Block(Pos(2, 0), Pos(2, 1)), Left),
        (Block(Pos(2, 3), Pos(2, 4)), Right)).toSet)
    }
  }

  test("Solver: done") {
    new Level1 {
      assert(done(Block(Pos(4, 7), Pos(4, 7))))
      assert(!done(Block(Pos(4, 6), Pos(4, 6))))
      assert(!done(Block(Pos(4, 7), Pos(4, 8))), "Block must be standing on the terminate point")
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution === optsolution)
      assert(solution.length === optsolution.length)
    }
  }

  trait Level3 extends SolutionChecker {

    val level =
      """------ooooooo--
    	|oooo--ooo--oo--
        |ooooooooo--oooo
        |oSoo-------ooTo
        |oooo-------oooo
        |------------ooo""".stripMargin
  }

  test("solution for level 3") {
    new Level3 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("solution length for level 3") {
    new Level3 {
      assert(solution === List(Right, Up, Right, Right, Right, Up, Left, Down, Right, Up, Up, Right, Right, Right, Down, Down, Down, Right, Up))
      assert(solution.length === List(Right, Up, Right, Right, Right, Up, Left, Down, Right, Up, Up, Right, Right, Right, Down, Down, Down, Right, Up).length)
    }
  }
}
