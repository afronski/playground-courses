package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int
  val BufferDelay: Int = 0

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println("  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }

    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }

    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }

    a1 addAction orAction
    a2 addAction orAction
  }


  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val b, c, d = new Wire

    inverter(a1, b)
    inverter(a2, c)

    andGate(b, c, d)

    inverter(d, output)
  }

  def buffer(input: Wire, output: Wire) {
    def doAction() {
      val inputSig = input.getSignal
      afterDelay(BufferDelay) { output.setSignal(inputSig) }
    }

    input addAction doAction
  }

  def demux(in: Wire, addresses: List[Wire], out: List[Wire]) {
    def createSubDemultiplexer(in: Wire, addresses: List[Wire], out: List[Wire]) {
      def handleStep(newIn: Wire, actualAddress: Wire, out1: Wire, out0: Wire) {
        val invertedActualAddress = new Wire

        inverter(actualAddress, invertedActualAddress)
        andGate(newIn, invertedActualAddress, out0)
        andGate(newIn, actualAddress, out1)
      }

      out match {
        case output :: Nil => buffer(in, output)
        case _ => {
          val internal1 = new Wire
          val internal2 = new Wire

          handleStep(in, addresses.head, internal1, internal2)

          val (upperBound, lowerBound) = out.splitAt(out.size / 2)

          createSubDemultiplexer(internal1, addresses.tail, upperBound)
          createSubDemultiplexer(internal2, addresses.tail, lowerBound)
        }
      }
    }

    createSubDemultiplexer(in, addresses, out)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in0, in1, out = new Wire

    andGate(in0, in1, out)

    probe("in0", in0)
    probe("in1", in1)
    probe("out", out)

    in0.setSignal(false)
    in1.setSignal(false)
    run

    in0.setSignal(true)
    run

    in1.setSignal(true)
    run
  }

  def orGateExample {
    val in0, in1, out = new Wire

    orGate(in0, in1, out)

    probe("in0", in0)
    probe("in1", in1)
    probe("out", out)

    in0.setSignal(false)
    in1.setSignal(false)
    run

    in0.setSignal(true)
    run

    in1.setSignal(true)
    run
  }

  def demuxExample {
    val in, address, out0, out1 = new Wire

    demux(in, List(address), List(out1, out0))

    probe("in", in)
    probe("address", address)
    probe("out0", out0)
    probe("out1", out1)

    in.setSignal(false)
    address.setSignal(false)
    run

    in.setSignal(false)
    address.setSignal(true)
    run

    in.setSignal(true)
    address.setSignal(false)
    run

    in.setSignal(true)
    address.setSignal(true)
    run
  }
}

object CircuitMain extends App {
  Circuit.andGateExample
}