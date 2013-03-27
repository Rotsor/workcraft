package org.workcraft.services

import java.awt.geom.Point2D
import org.workcraft.scala.effects.IOMonad
import org.workcraft.scala.effects.IO
import java.awt.geom.Rectangle2D
import java.awt.geom.AffineTransform

object LayoutableService extends Service[ModelScope, Layoutable]

sealed abstract class LayoutOrientation (val transform: AffineTransform)

object LayoutOrientation {
  case object Up extends LayoutOrientation(new AffineTransform())
  case object Down extends LayoutOrientation (AffineTransform.getScaleInstance(1, -1))
  case object LeftToRight extends LayoutOrientation (AffineTransform.getRotateInstance(scala.math.Pi / 2))
  case class Custom (t: AffineTransform) extends LayoutOrientation(t) 
}

trait Layouter[M[_]] {
  def layout[N](spec : LayoutSpec[N]) : M [List[(N, Point2D.Double)]]
}

trait Layoutable {
  def apply[M[_]](layouter : Layouter[M])(implicit m : IOMonad[M]): M[Unit]
}

case class LayoutSpec[Node] (
  nodes: List[Node],
  size: Node => (Double, Double),  // (width, height)
  outgoingArcs: Node => List[Node],
  nodeSeparation: Double,
  rankSeparation: Double,
  orientation: LayoutOrientation)
