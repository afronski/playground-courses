package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val deathRate = 0.25
    val infectionRate = 0.4

    val airplanes = false
    val airplaneProbability = 0.01

    val chosenFew = false
    val vipRate = 0.05

    val reduceMobility = false
  }

  import SimConfig._

  val infectedAtStart = (population * prevalenceRate).toInt
  val healthyEntities = population - infectedAtStart

  val healthy: List[Person] =
    (for (i <- 1 to healthyEntities)
      yield new Person(i)).toList

  val infected: List[Person] =
    (for (i <- 1 to infectedAtStart)
      yield new Person(healthyEntities + i)).toList

  infected foreach { _.infect }

  val persons = healthy ::: infected

  def forPeople(row: Int, col: Int, action: (Person) => Boolean): Boolean = {
    def iteratePeople(people: List[Person]): Boolean = people match {
      case x :: xs =>
        if (x.row == row && x.col == col && action(x)) {
          true
        } else {
          iteratePeople(xs)
        }

      case Nil => {
        false
      }
    }

    iteratePeople(persons)
  }

  def isDangerous(row: Int, col: Int): Boolean = {
    forPeople(row, col, x => x.sick || x.dead)
  }

  def isContagious(row: Int, col: Int): Boolean = {
    forPeople(row, col, x => x.infected || x.sick || x.dead)
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    var nextRow: Int = row
    var nextCol: Int = col

    def scheduleMove {
      val value = randomBelow(5) + 1

      if (value <= 0 || value > 5) {
        println(value)
      }

      afterDelay(if (reduceMobility) (if (sick) value * 4 else value * 2) else value)(move)
    }

    def move {
      if (!dead) {
        def testMove(newRow: Int, newCol: Int): Boolean = {
          if (!isDangerous(newRow, newCol)) {
            nextRow = newRow
            nextCol = newCol

            if (airplanes && random <= airplaneProbability) {
              nextRow = randomBelow(roomRows)
              nextCol = randomBelow(roomColumns)
            }

            applyDecisions
            true
          }

          false
        }

        val moves = scala.util.Random.shuffle(
          List(
            ((row - 1 + roomRows) % roomRows, col),
            ((row + 1)            % roomRows, col),
            (row, (col + 1)               % roomColumns),
            (row, (col - 1 + roomColumns) % roomColumns)
          )
        )

        def testMoves(): Unit = for(m <- moves) {
          if (testMove(m._1, m._2)) {
            return
          }
        }

        testMoves()
      }

      scheduleMove
    }

    def applyDecisions {
      row = nextRow
      col = nextCol

      if (isContagious(row, col) && random <= infectionRate) {
        infect
      }
    }

    def getSick {
      sick = true
    }

    def die {
      if (random <= deathRate) {
        dead = true
      }
    }

    def immunize {
      if (dead) {
        return
      }

      sick = false
      immune = true
    }

    def reset {
      if (dead) {
        return
      }

      infected = false
      sick = false
      immune = false
    }

    def infect {
      if (immune || dead || sick || infected) {
        return
      }

      infected = true

      afterDelay(6)(getSick)
      afterDelay(14)(die)
      afterDelay(16)(immunize)
      afterDelay(18)(reset)
    }

    scheduleMove

    if (chosenFew && random <= vipRate) {
      immune = true
    }
  }
}