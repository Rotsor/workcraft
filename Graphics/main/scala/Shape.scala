package org.workcraft.graphics

import java.awt.Color
import java.awt.geom.Line2D
import java.awt.geom.PathIterator
import java.awt.geom.Point2D
import java.awt.BasicStroke
import java.awt.Stroke

import Java2DDecoration._

class Shape private (val s: java.awt.Shape, val stroke: Option[(Stroke, Color)], val fillColor: Option[Color]) {
  
  lazy val colorisableGraphicalContent = new ColorisableGraphicalContent {
    override def draw(r: DrawRequest) = {
      val g = r.graphics
      val colorisation = r.colorisation.foreground

      for (color <- fillColor) {
        g.setColor(Coloriser.colorise(color, colorisation))
        g.fill(s)
      }

      for ((stroke, color) <- stroke) {
        g.setStroke(stroke)
        g.setColor(Coloriser.colorise(color, colorisation))
        g.draw(s)
      }
    }
  }

  lazy val graphicalContent = colorisableGraphicalContent.applyColorisation(Colorisation.Empty)
    
  lazy val boundedColorisableGraphicalContent = BoundedColorisableGraphicalContent (colorisableGraphicalContent, BoundingBox(s.bounds))
  
  lazy val touchable = new Touchable {

     val pathError = 0.01
     val segments = getSegments(s.getPathIterator(null, pathError))
 
    private def testSegments(segments: List[Line2D.Double], point: Point2D.Double, threshold: Double): Boolean = {
       val tSq = threshold * threshold
      for (s <- segments) {
        if (s.ptSegDistSq(point) < tSq)
         return true
      }
      false
    }

    private def getSegments(i: PathIterator): List[Line2D.Double] = {
      val coords = new Array[Double](6)

      var curX = 0.0
      var curY = 0.0

      var startX = 0.0
      var startY = 0.0

      var broken = true

      var segments: List[Line2D.Double] = Nil

      while (!i.isDone) {
        val t = i.currentSegment(coords)
        if (t == PathIterator.SEG_MOVETO) {
          curX = coords(0)
          curY = coords(1)
          broken = true
        } else if (t == PathIterator.SEG_LINETO) {
          segments = new Line2D.Double(curX, curY, coords(0), coords(1)) :: segments
          if (broken) {
            startX = curX
            startY = curY
            broken = false
          }
          curX = coords(0)
          curY = coords(1)
        } else if (t == PathIterator.SEG_CLOSE) {
          segments = new Line2D.Double(curX, curY, startX, startY) :: segments
          curX = startX
          curY = startY
          broken = true
        }

        i.next()
      }

      segments
     }
    def hitTest(point: Point2D.Double) = testSegments(segments, point, 0.1) || s.contains(point)
    def boundingBox = BoundingBox(s.bounds)
  }
}

object Shape {
  def apply (s: java.awt.Shape, stroke: Option[(Stroke, Color)], fillColor: Option[Color]) = new Shape(s, stroke, fillColor) 
}
