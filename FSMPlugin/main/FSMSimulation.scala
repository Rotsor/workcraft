package org.workcraft.plugins.fsm
import org.workcraft.gui.modeleditor.sim.SimulationModel
import org.workcraft.dependencymanager.advanced.user.Variable
import org.workcraft.scala.Expressions._
import org.workcraft.scala.effects.IO
import scalaz.Scalaz._

case class FSMSimulation(fsm: FSM, input: List[String]) extends SimulationModel [Arc, (State, List[String])] {
  private val curState = Variable.create((fsm.initialState, input))
  
  val state = curState.expr

  val enabled = state.map { case (curState, in) => (a: Arc) => {
    val arcs = fsm.postset(curState).filter(_._2 == a)

    arcs.exists(a =>
      fsm.arcLabels(a._2).list.contains(None) ||
        (in match {
	  case Nil => false
	  case (x :: _) => fsm.arcLabels(a._2).list.contains(Some(x))
        }))
    }
  }

  def fire(s: Arc) = curState.update { case (_, input) => (s.to, if (fsm.arcLabels(s) == "") input else input.tail) }
    
  def name (e: Arc) = fsm.labels(e.to)

  def setState(state: (State, List[String])) = curState.set(state)
}
