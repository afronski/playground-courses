package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("Test for population amount") {
    val es = new EpidemySimulator

    assert(es.persons.length == es.SimConfig.population,
           "Population amount should be equal to value set up in config.")
  }

  test("Test for prevalence rate") {
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate, "Prevalence rate should be 0.01.")
  }

  test("Test for requirement about dead person, which stays dead") {
    val es = new EpidemySimulator

    val chosenOne = es.persons.head

    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val (row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state.")
      assert(chosenOne.infected == true, "Dead person keeps infected.")
      assert(chosenOne.immune == false, "Dead person cannot become immune.")
      assert(chosenOne.sick == true, "Dead person keeps sick.")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move.")
    }
  }

  test("Test for moving") {
    for(i <- 1 to 100) {
      val es = new EpidemySimulator
      val chosenOne = es.persons.find(p => !p.dead).get

      val row = chosenOne.row
      val col = chosenOne.col
      var moves = 0

      while (es.agenda.head.time <= 5) {
        es.next

        if (chosenOne.row != row || chosenOne.col != col) {
          moves = moves + 1
        }
      }

      val nextRow = (row + 1) % es.SimConfig.roomRows
      val prevRow = (row - 1 + es.SimConfig.roomRows) % es.SimConfig.roomRows

      val nextCol = (col + 1) % es.SimConfig.roomColumns
      val prevCol = (col - 1 + es.SimConfig.roomColumns) % es.SimConfig.roomColumns

      if (!es.isDangerous(nextRow, col) && !es.isDangerous(prevRow, col) &&
          !es.isDangerous(row, prevCol) && !es.isDangerous(row, nextCol)) {
        assert(chosenOne.row != row || chosenOne.col != col || moves > 1, "Should move within 5 days.")
      }
    }
  }

  test("Test for no moving when surrounded by sick") {
    val es = new EpidemySimulator
    val chosenOne = es.persons.head

    val row = chosenOne.row
    val col = chosenOne.col

    val upperRow = (chosenOne.row - 1 + es.SimConfig.roomRows) % es.SimConfig.roomRows
    val belowRow = (chosenOne.row + 1) % es.SimConfig.roomRows

    val leftCol = (chosenOne.col - 1 + es.SimConfig.roomColumns) % es.SimConfig.roomColumns
    val rightCol = (chosenOne.col + 1) % es.SimConfig.roomColumns

    val upperRoom = es.persons.find(p => p.row == upperRow && p.col == chosenOne.col).get
    val belowRoom = es.persons.find(p => p.row == belowRow && p.col == chosenOne.col).get

    val leftRoom = es.persons.find(p => p.row == chosenOne.row && p.col == leftCol).get
    val rightRoom = es.persons.find(p => p.row == chosenOne.row && p.col == rightCol).get

    leftRoom.sick = true
    leftRoom.dead = true

    rightRoom.sick = true
    rightRoom.dead = true

    upperRoom.sick = true
    upperRoom.dead = true

    belowRoom.sick = true
    belowRoom.dead = true

    while (es.agenda.head.time <= 5) {
      es.next
    }

    assert(chosenOne.row == row && chosenOne.col == col, "Should stay in place.")
  }

  test("Test for no moving when surrounded by dead") {
    val es = new EpidemySimulator
    val chosenOne = es.persons.head

    val row = chosenOne.row
    val col = chosenOne.col

    val upperRow = (chosenOne.row - 1 + es.SimConfig.roomRows) % es.SimConfig.roomRows
    val belowRow = (chosenOne.row + 1) % es.SimConfig.roomRows

    val leftCol = (chosenOne.col - 1 + es.SimConfig.roomColumns) % es.SimConfig.roomColumns
    val rightCol = (chosenOne.col + 1) % es.SimConfig.roomColumns

    val upperRoom = es.persons.find(p => p.row == upperRow && p.col == chosenOne.col).get
    val belowRoom = es.persons.find(p => p.row == belowRow && p.col == chosenOne.col).get

    val leftRoom = es.persons.find(p => p.row == chosenOne.row && p.col == leftCol).get
    val rightRoom = es.persons.find(p => p.row == chosenOne.row && p.col == rightCol).get

    leftRoom.dead = true
    rightRoom.dead = true
    upperRoom.dead = true
    belowRoom.dead = true

    while (es.agenda.head.time <= 5) {
      es.next
    }

    assert(chosenOne.row == row && chosenOne.col == col, "Should stay in place.")
  }

  test("Test for infection after moving to wrong room") {
    val es = new EpidemySimulator
    val chosenOne = es.persons.head

    val row = chosenOne.row
    val col = chosenOne.col

    val upperRow = (chosenOne.row - 1 + es.SimConfig.roomRows) % es.SimConfig.roomRows
    val belowRow = (chosenOne.row + 1) % es.SimConfig.roomRows

    val leftCol = (chosenOne.col - 1 + es.SimConfig.roomColumns) % es.SimConfig.roomColumns
    val rightCol = (chosenOne.col + 1) % es.SimConfig.roomColumns

    val upperRoom = es.persons.find(p => p.row == upperRow && p.col == chosenOne.col).get
    val belowRoom = es.persons.find(p => p.row == belowRow && p.col == chosenOne.col).get

    val leftRoom = es.persons.find(p => p.row == chosenOne.row && p.col == leftCol).get
    val rightRoom = es.persons.find(p => p.row == chosenOne.row && p.col == rightCol).get

    leftRoom.infected = true
    leftRoom.sick = false
    leftRoom.dead = false
    leftRoom.immune = false

    rightRoom.infected = true
    rightRoom.sick = false
    rightRoom.dead = false
    rightRoom.immune = false

    upperRoom.infected = true
    upperRoom.sick = false
    upperRoom.dead = false
    upperRoom.immune = false

    belowRoom.infected = true
    belowRoom.sick = false
    belowRoom.dead = false
    belowRoom.immune = false

    assert(chosenOne.infected == false, "Shouldn't be infected at start.")

    while (es.agenda.head.time <= 5) {
      es.next
    }

    assert(chosenOne.row != row || chosenOne.col != col, "Should move in one of the rooms.")
    assert(chosenOne.infected == true, "Should be infected as well.")
  }

  test("Test for life cycle") {
    val es = new EpidemySimulator

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val infectedPerson = (es.persons.find{_.infected}).get

    while (es.agenda.head.time < incubationTime) {
      assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days.")
      assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days.")
      assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days.")
      assert(infectedPerson.dead == false, "Infected person does not die in 6 days.")

      es.next
    }

    assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time.")

    while (es.agenda.head.time == incubationTime) {
      es.next
    }

    assert(infectedPerson.sick == true, "Infected person should become sick after 6 days.")

    while (es.agenda.head.time < dieTime) {
      assert(infectedPerson.infected == true, "Sick person keeps infected.")
      assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune.")
      assert(infectedPerson.immune == false, "Sick person is not immune.")
      assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days.")

      es.next
    }

    assert(es.agenda.head.time == dieTime,
           "You should set a 'die' event (decides with a probability 25%) after 14 days.")
  }

  test("Test for transmissibility rate") {
    var infectedTimes = 0

    for (i <- 0 to 100) {
      val es = new EpidemySimulator
      val healthyPerson = (es.persons find {p => !p.infected}).get

      es.persons.filter(p => p != healthyPerson) foreach {_.infected = true}

      while (es.agenda.head.time < 6) {
        es.next
      }

      infectedTimes = infectedTimes + (if (healthyPerson.infected) 1 else 0)
    }

    assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate.")
  }
}