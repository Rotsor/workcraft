package org.workcraft.plugins.fsm
import org.workcraft.gui.modeleditor.sim.SimulationModel
import org.workcraft.dependencymanager.advanced.user.Variable
import org.workcraft.scala.Expressions._
import scalaz.effect.IO
import scalaz.Scalaz._

case class FSMSimulationGen(fsm: FSM) extends SimulationModel [Arc, (State, List[String])] {
  private val curState = Variable.create((fsm.initialState, List[String]()))
  
  val state = curState.expr

  val enabled = state.map { case (curState, in) => (a: Arc) => fsm.postset(curState).exists(_._2 == a) }
  
  def fire(s: Arc) = curState.update { 
    case (_, input) => (s.to, 
      input ++ List(ArcLabels.printLabels(" OR ")(fsm.arcLabels(s))))
  }
    
  def name (e: Arc) = fsm.labels(e.to)

  def setState(state: (State, List[String])) = curState.set(state)
}
