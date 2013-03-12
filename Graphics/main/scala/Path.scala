package org.workcraft.graphics

import java.awt.Color
import java.awt.BasicStroke
import java.awt.geom.PathIterator
import java.awt.geom.Point2D
import java.awt.geom.Line2D
import java.awt.geom.Path2D

import Java2DDecoration._

class Path private (
  val points: List[Point], 
  val closed : Boolean,
  val stroke: Option[(BasicStroke, Color)], 
  val fill : Option[Color]) {

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
    if(closed)
      res.closePath
    res
  }

  lazy val colorisableGraphicalContent = ColorisableGraphicalContent(colorisation => GraphicalContent ( g => {
    fill.foreach {
      fill => {
        g.setColor(Coloriser.colorise(fill, colorisation.foreground))
        g.fill(path2D)
      }
    }
    stroke.foreach{
      case (s, c) => {
        g.setStroke(s)
        g.setColor(Coloriser.colorise(c, colorisation.foreground))
        g.draw(path2D)
      }
    }
  }))

  lazy val graphicalContent = colorisableGraphicalContent.applyColorisation(Colorisation.Empty)
  
  lazy val boundedColorisableGraphicalContent = BoundedColorisableGraphicalContent (colorisableGraphicalContent, BoundingBox(path2D.bounds))
  
  def touchable(touchThreshold: Double) = new Touchable {
    val pathError = 0.01

    private def testSegments(point: Point2D.Double, threshold: Double): Boolean = {
      val segments = points.zip(points match { 
        case h :: t => t ++ (if(closed)List(h) else Nil);
        case Nil => Nil 
      }).
        map{case (a, b) => line2D(a, b)}
      val tSq = threshold * threshold
      !segments.find(s => s.ptSegDistSq(point) < tSq).isEmpty
    }
    def hitTest(point: Point2D.Double) = testSegments(point, touchThreshold)
    def boundingBox = BoundingBox(path2D.bounds)
  }
}

object Path {
  def apply(p: List[Point], closed : Boolean, stroke: Option[(BasicStroke, Color)], fill: Option[Color]) = 
    new Path (p, closed, stroke, fill)
}
