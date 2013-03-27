package org.workcraft.plugins.petri2

import org.workcraft.scala.Expressions._
import scalaz.Scalaz._
import java.awt.geom.Rectangle2D
import java.awt.geom.Point2D
import org.workcraft.scala.effects.IOMonad
import org.workcraft.services.LayoutSpec
import org.workcraft.services.Layoutable
import org.workcraft.services.Layouter
import org.workcraft.services.LayoutOrientation
import org.workcraft.gui.CommonVisualSettings

object PetriNetLayoutable {
  class PetriNetLayoutNode

  def apply(net: EditablePetriNet) : Layoutable = new Layoutable {
    def apply[M[_]](layouter : Layouter[M])(implicit m : IOMonad[M]) = {
      (m lift (
      (CommonVisualSettings.settings.map(_.size) <**>
      net.saveState){case (size, vpn) => {

        val components = vpn.net.places ++ vpn.net.transitions

        val outgoingArcs = (n: Component) => vpn.net.postset(n)
    
        layouter.layout(LayoutSpec(
          components,
          (_ : Component) => (size, size),
          outgoingArcs,
          2,
          3,
          LayoutOrientation.LeftToRight))
          .flatMap((l : List[(Component, Point2D.Double)]) => {
            l.traverse_ { case (n, p) => m.lift(net.layout.update ( _ + (n -> p))) } >| {}
      })
      }
    }.eval)).flatMap(x => x)
    }
  }
}
