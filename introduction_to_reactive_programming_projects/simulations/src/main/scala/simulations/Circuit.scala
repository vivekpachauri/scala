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

  def probe(name: String, wire: Wire) {
    wire addAction {
      () =>
        afterDelay(0) {
          println(
            "  " + currentTime + ": " + name + " -> " + wire.getSignal)
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

  def nandGate(a1: Wire, a2: Wire, output: Wire) {
    def nandAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay + InverterDelay) { output.setSignal(!(a1Sig & a2Sig)) }
    }
    a1 addAction nandAction
    a2 addAction nandAction
  }

  //
  // to complete with orGates and demux...
  //
  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val nand1Output, nand2Output = new Wire
    /* logic for or gate using nand - apply nand to first input, apply nand to second input,
     * run the two resulting outputs through another nand
     */
    nandGate(a1, a1, nand1Output)
    nandGate(a2, a2, nand2Output)
    nandGate(nand1Output, nand2Output, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def demuxHelper(in: Wire, c: List[Wire], out: List[Wire], resultingWire: Wire) {
      /* given that the list contains elements in decreasing indexes therefore the frist one is the n-1 index and the last is 0 index */
      /* also need to keep in mind that the base case is that there are no selectors and therefore there is one output wire,
         * which gets the high signal of the input wire */
      /* the next case is when there is one control wire and therefore 2 output wire,
          * if the control wire is zero then the high signal goes to output zero else if the control wire is high then the
          * high signal goes to the output one*/
      c match {
        case Nil => /* set the signal of input on to output ???which index of output??? let's try assuming that there is only one element in output*/
          /* if the selector is empty then there is only one output */
          /* ***********************************
           * ************************************
           * important - should I use the and gate or should I use an action which will set the signal in the wire
           *  ************************************
           */
          /*          val temp = new Wire
          temp.setSignal(in.getSignal)
          andGate(in, temp, out(0))*/
          andGate(in, resultingWire, out(0))
        /* just for the hell let's create one case for single selector as well !!!! will need 
         * to take this case out later otherwise this will become the base case!!!!*/
//        case fst :: Nil =>
        /*
           * one selector and therefore two outputs
           */
        /*
           * the two options of the selector don't need to be iterated upon, but we do have to iterate over the output wires to set the right output
           * two cases: for the output wire with index mod 2 equals 0 (index 0) set the output to ~s0 & whatever
           * second case - for the output wire with index mod 2 not equals 0 (index 1) set the output to s0 & whatever (need to re-think this whatever)
           * whatever part: recurse with input, one less selector, and either the first or second half of the output
           */

        case fst :: rst =>
          val (firstList, secondList) = out.splitAt(out.length / 2)
          /* now i need to recurse with one less selector and only one of the two of these lists */
          /* resulting wire will be updated according to the first or the second half of the list if recursed upon */
          val temp = new Wire
          val newResultFirstHalf = new Wire
          val newResultSecondHalf = new Wire
          inverter(fst, temp)
          /* now and the out with this inverted wire */
          /*          for (o <- firstList) andGate(temp, in, o)
          for (o <- secondList) andGate(fst, in, o)*/
          andGate(resultingWire, temp, newResultFirstHalf)
          andGate(resultingWire, fst, newResultSecondHalf)
          demuxHelper(in, rst, firstList, newResultFirstHalf)
          demuxHelper(in, rst, secondList, newResultSecondHalf)
      }
    }
    val firstResultingWire = new Wire
    firstResultingWire.setSignal(true)
    demuxHelper(in, c, out, firstResultingWire)
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
