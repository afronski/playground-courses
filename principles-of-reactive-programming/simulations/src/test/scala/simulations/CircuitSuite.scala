package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("Testing NOT gate logic") {
    val in, out = new Wire

    inverter(in, out)

    in.setSignal(false)
    run

    assert(out.getSignal === true, "~0 = 1")

    in.setSignal(true)
    run

    assert(out.getSignal === false, "~1 = 0")

    in.setSignal(false)
    run

    assert(out.getSignal === true, "Returning to the previous state.")
  }

  test("Testing AND gate logic") {
    val in0, in1, out = new Wire

    andGate(in0, in1, out)

    in0.setSignal(false)
    in1.setSignal(false)
    run

    assert(out.getSignal === false, "0 & 0 = 0")

    in0.setSignal(true)
    run

    assert(out.getSignal === false, "1 & 0 = 0")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "1 & 1 = 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "0 & 1 = 0")
  }

  test("Testing OR gate logic") {
    val in0, in1, out = new Wire

    orGate(in0, in1, out)

    in0.setSignal(false)
    in1.setSignal(false)
    run

    assert(out.getSignal === false, "0 | 0 = 0")

    in0.setSignal(true)
    run

    assert(out.getSignal === true, "1 | 0 = 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "1 | 1 = 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "0 | 1 = 1")
  }

  test("Testing complicated OR gate logic (built from AND and NOT gates)") {
    val in0, in1, out = new Wire

    orGate2(in0, in1, out)

    in0.setSignal(false)
    in1.setSignal(false)
    run

    assert(out.getSignal === false, "0 | 0 = 0")

    in0.setSignal(true)
    run

    assert(out.getSignal === true, "1 | 0 = 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "1 | 1 = 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "0 | 1 = 1")
  }

  test("Testing 0:1 demultiplexer logic") {
    val in, out = new Wire

    demux(in, List(), List(out))

    in.setSignal(false)
    run

    assert(out.getSignal === false, "in -> out")

    in.setSignal(true)
    run

    assert(out.getSignal === true, "in -> out")
  }

  test("Testing 1:2 demultiplexer logic") {
    val in, out0, out1, address = new Wire

    demux(in, List(address), List(out1, out0))

    in.setSignal(false)
    address.setSignal(false)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)

    in.setSignal(false)
    address.setSignal(true)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)

    in.setSignal(true)
    address.setSignal(false)
    run

    assert(out0.getSignal === true)
    assert(out1.getSignal === false)

    in.setSignal(true)
    address.setSignal(true)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === true)
  }

  test("Testing 2:4 demultiplexer logic") {
    val in, out0, out1, out2, out3, address0, address1 = new Wire

    demux(in, List(address1, address0), List(out3, out2, out1, out0))

    // Passing '0'.

    in.setSignal(false)
    address0.setSignal(false)
    address1.setSignal(false)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)

    in.setSignal(false)
    address0.setSignal(true)
    address1.setSignal(false)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)

    in.setSignal(false)
    address0.setSignal(false)
    address1.setSignal(true)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)

    in.setSignal(false)
    address0.setSignal(true)
    address1.setSignal(true)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)

    // Passing '1'.

    in.setSignal(true)
    address0.setSignal(false)
    address1.setSignal(false)
    run

    assert(out0.getSignal === true)
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)

    in.setSignal(true)
    address0.setSignal(true)
    address1.setSignal(false)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === true)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)

    in.setSignal(true)
    address0.setSignal(false)
    address1.setSignal(true)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)
    assert(out2.getSignal === true)
    assert(out3.getSignal === false)

    in.setSignal(true)
    address0.setSignal(true)
    address1.setSignal(true)
    run

    assert(out0.getSignal === false)
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === true)
  }
}