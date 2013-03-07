package org.workcraft.graphics

import java.awt.Color
import java.awt.BasicStroke
import java.awt.geom.PathIterator
import java.awt.geom.Point2D
import java.awt.geom.Line2D
import java.awt.geom.Path2D

import Java2DDecoration._

class Path private (val points: List[Point], val closed : Boolean, val stroke: BasicStroke, val color: Color) {

  def path2D : Path2D.Double = {
    val res = new Path2D.Double()
    points match {
      case head :: tail => {
        res.moveTo(head.x, head.y)
        for(p <- tail) {
          res.lineTo(p.x, p.y)
        }
      }
      case _ => {}
    }
    res
  }

  lazy val graphicalContent = GraphicalContent( g => {
      g.setStroke(stroke)
      g.setColor(color)
      g.draw(path2D)
  })
  
  lazy val colorisableGraphicalContent = ColorisableGraphicalContent(colorisation => GraphicalContent ( g => {
      g.setStroke(stroke)
      g.setColor(Coloriser.colorise(color, colorisation.foreground))
      g.draw(path2D)
  }))
  
  lazy val boundedColorisableGraphicalContent = BoundedColorisableGraphicalContent (colorisableGraphicalContent, BoundingBox(path2D.bounds))
  
  def touchable(touchThreshold: Double) = new Touchable {
    val pathError = 0.01

    private def testSegments(point: Point2D.Double, threshold: Double): Boolean = {
      val segments = points.zip(points match { case h :: t => t ++ List(h); case Nil => Nil }).
        map{case (a, b) => line2D(a, b)}
      val tSq = threshold * threshold
      !segments.find(s => s.ptSegDistSq(point) < tSq).isEmpty
    }
    def hitTest(point: Point2D.Double) = testSegments(point, touchThreshold)
    def boundingBox = BoundingBox(path2D.bounds)
  }
}

object Path {
  def apply(p: List[Point], closed : Boolean, stroke: BasicStroke, color: Color) = 
    new Path (p, closed, stroke, color)
}
