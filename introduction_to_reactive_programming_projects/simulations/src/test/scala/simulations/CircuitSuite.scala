package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal == false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal == true, "or 2")
    
    in2.setSignal(true)
    run
    assert(out.getSignal == true, "or 3")
    
  }
  
  test("demux example"){
    val in1, out = new Wire
    in1.setSignal(false)
    demux(in1, Nil, List(out))
    run
    assert(out.getSignal == false)
    
    in1.setSignal(true)
    demux(in1, Nil, List(out))
    run
    assert(out.getSignal == true)
    
    /* test with one selector set to 0*/
    val s0, s1, s2 = new Wire
    s0.setSignal(true)
    var outsAsSeq = for {
      i <- (0 to 1)
    } yield new Wire
    var outs = outsAsSeq.toList
    demux(in1, List(s0), outs)
    run
    println("with one selector")
    outs map (x=>println(x.getSignal))
    
    s0.setSignal(false)
    s1.setSignal(true)
    outsAsSeq = for {
      i <- (0 to 3)
    } yield new Wire
    outs = outsAsSeq.toList
    demux(in1, List(s1, s0), outs)
    run
    println("with two selector")
    println("0 -> " + outs(0).getSignal)
    println("1 -> " + outs(1).getSignal)
    println("2 -> " + outs(2).getSignal)
    println("3 -> " + outs(3).getSignal)
//    outs map (x=>println(x.getSignal))
    /*    assert(outs(0).getSignal == true)
    assert(outs(1).getSignal == false)*/
    println("with three selectors")
        outsAsSeq = for {
      i <- (0 to 7)
    } yield new Wire
    outs = outsAsSeq.toList
    s0.setSignal(false)
    s1.setSignal(false)
    s2.setSignal(true)
    demux(in1, List(s2, s1, s0), outs)
    run
    println("with one selector")
    outs map (x=>println(x.getSignal))
  }

}
