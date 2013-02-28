package org.workcraft.plugins.fsm

import org.scalatest.Spec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DeterminisationTest extends Spec {
  describe("determiniser") {
    it("should work") {
      val nfa1 = NFA[Int, Int](1, {
        case 1 => List((Some(0),2), (None,3))
        case 2 => List((Some(1),2), (Some(1),4))
        case 3 => List((None, 2), (Some(0), 4))
        case 4 => List((Some(0), 3))
      }, {
        case 3 => true
        case 4 => true
        case _ => false
      })
      val dfa1 = Determinisation.determinise(nfa1)
      val s123 = Set(1,2,3)
      val s24 = Set(2,4)
      val s23 = Set(2,3)
      val s4 = Set(4)
      val expectedData = Set (
        (s123, Map((0, s24), (1, s24)), true),
        (s23, Map((0, s4), (1, s24)), true),
        (s24, Map((0, s23), (1, s24)), true),
        (s4, Map((0, s23)), true)
      )
      println(expectedData)
      println(dfa1.allData)
      assert(expectedData == dfa1.allData.toSet)
    }
  }
}
