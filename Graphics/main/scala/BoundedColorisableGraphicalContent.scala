package org.workcraft.graphics

import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import java.awt.geom.AffineTransform

import Graphics._
import Java2DDecoration._

class BoundedColorisableGraphicalContent(val cgc: ColorisableGraphicalContent, val bounds: BoundingBox) {
  
  def transform(transform: AffineTransform) = {
    BoundedColorisableGraphicalContent(
      cgc.transform(transform),
      bounds.transform(transform))
  }

  def centerToBoundingBox = transform(translation(-bounds.visual.center))
  
  def compose(top: BoundedColorisableGraphicalContent) =
    BoundedColorisableGraphicalContent(
      cgc.compose(top.cgc),
      bounds.union(top.bounds))
      
  def align (to: BoundedColorisableGraphicalContent, horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment): BoundedColorisableGraphicalContent =
    transform(alignTransform(bounds.visual, to.bounds.visual, horizontalAlignment, verticalAlignment))

  def alignSideways (relativeTo: BoundedColorisableGraphicalContent, position: LabelPositioning): BoundedColorisableGraphicalContent =
    transform(LabelPositioning.positionRelative(bounds.visual, relativeTo.bounds.visual, position))
}

object BoundedColorisableGraphicalContent {
  def apply(cgc: ColorisableGraphicalContent, bounds: BoundingBox) = new BoundedColorisableGraphicalContent(cgc, bounds)
}
