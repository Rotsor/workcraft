package org.workcraft.plugins.fsm

import org.workcraft.gui.MainWindow
import org.workcraft.gui.services.GuiTool
import org.workcraft.gui.services.ToolClass
import scalaz.effect.IO._
import org.workcraft.scala.Expressions._
import org.workcraft.scala.Scalaz._
import org.workcraft.dom.visual.connections._
import java.awt.geom.Point2D
import Services._
import scalaz._
import Scalaz._


object DeterminisationTool extends GuiTool {
  val classification = ToolClass.Conversion
  val description = "Determinize"
  def run(mainWindow: MainWindow) = mainWindow.editorInFocus.expr.map(_.flatMap(_.content.model.implementation(NfaService)) match {
    case Some(getOrigNfa) => Some(for(
      lNfa <- getOrigNfa;
      dfa = Determinisation.determinise[String, String](lNfa);
      pstateLabel = (p : Set[String]) => "{" + p.toList.sortWith((x,y)=>x<y).mkString(",") + "}";
      parseStateLabel = (s : String) => dfa.allStates.toList.map(s => (pstateLabel(s), s)).toMap.get(s).get;
      result = dfa.nfa.mapStates(pstateLabel, parseStateLabel);
      fsm <- FSM.create(result);
      efsm <- EditableFSM.create(VisualFSM(
        fsm, 
        fsm.states.map(s => (s, new Point2D.Double(0,0))).toMap, 
        fsm.arcs.map(a => (a, Polyline(List()))).toMap
      ));
      result <- mainWindow.newModel(FSMModel(efsm), true)
    ) yield result
    )
    case None => None
  }
  )
}
