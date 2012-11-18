package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
 * inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
 * inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  val level: String

  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean =
    (xy: Pos) =>
      xy.x >= 0 &&
      xy.y >= 0 &&
      xy.x < levelVector.length &&
      xy.y < levelVector(0).length &&
      levelVector(xy.x)(xy.y) != '-'

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val rowIndex = levelVector.indexWhere(row => row.contains(c))
    Pos(rowIndex, levelVector(rowIndex).indexOf(c))
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)
}
