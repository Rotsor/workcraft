package org.workcraft.plugins.fsm
import java.awt.geom.Point2D
import org.workcraft.scala.Expressions._
import scalaz.effect.MonadIO
import scalaz.Scalaz._
import org.workcraft.services.layout.Layoutable
import org.workcraft.services.layout.Layouter
import org.workcraft.services.layout.LayoutSpec
import org.workcraft.services.layout.LayoutOrientation
import org.workcraft.gui.CommonVisualSettings

object FSMLayoutable {
  def apply(efsm: EditableFSM) : Layoutable = new Layoutable {
    def apply[M[_]](layouter : Layouter[M])(implicit m : MonadIO[M]) = {
      m.liftIO((for(
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
          l => m.liftIO(l.traverse_ {
            p => efsm.layout.update(_ + p)
          })
        }
      }).eval).flatMap(x => x)
    }
  }
}
