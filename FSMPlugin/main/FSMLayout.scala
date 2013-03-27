package org.workcraft.plugins.fsm
import java.awt.geom.Point2D
import org.workcraft.scala.Expressions._
import org.workcraft.scala.effects.IOMonad
import scalaz.Scalaz._
import org.workcraft.services.Layoutable
import org.workcraft.services.Layouter
import org.workcraft.services.LayoutSpec
import org.workcraft.services.LayoutOrientation
import org.workcraft.gui.CommonVisualSettings

object FSMLayoutable {
  def apply(efsm: EditableFSM) : Layoutable = new Layoutable {
    def apply[M[_]](layouter : Layouter[M])(implicit m : IOMonad[M]) = {
      m.lift((for(
        vfsm <- efsm.saveState;
        size <- CommonVisualSettings.settings.map(_.size))
      yield {
        layouter.layout(LayoutSpec[State](
          vfsm.fsm.states,
          _ => (size, size), 
          n => vfsm.fsm.postset(n).map(p => p._1),
          2, 
          2, 
          LayoutOrientation.Down
        )) >>= { 
          l => m.lift(l.traverse_ { 
            p => efsm.layout.update(_ + p) 
          })
        }
      }).eval).flatMap(x => x)
    }
  }
}
