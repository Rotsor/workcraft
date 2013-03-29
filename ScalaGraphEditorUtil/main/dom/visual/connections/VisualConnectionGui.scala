package org.workcraft.dom.visual.connections

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Shape
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import org.workcraft.graphics.ColorisableGraphicalContent
import org.workcraft.graphics.Touchable
import org.workcraft.scala.Expressions._
import org.workcraft.graphics.DrawRequest
import org.workcraft.graphics.Coloriser
import org.workcraft.graphics.TouchableC
import org.workcraft.graphics.BoundingBox
import org.workcraft.graphics.PartialCurveInfo
import org.workcraft.graphics.VisualCurveProperties
import java.awt.geom.Path2D
import java.awt.geom.AffineTransform
import java.awt.BasicStroke
import org.workcraft.graphics.Geometry.buildConnectionCurveInfo
import org.workcraft.graphics._
import org.workcraft.graphics.Graphics._
import org.workcraft.graphics.Java2DDecoration._
import scalaz._

object VisualConnectionGui {

  val HitThreshold = 0.2

  def arrowHead(color: Color, headPosition: Point2D, orientation: Double, length: Double, width: Double) = {
    val arrowShape = List(
      point2D(-length, -width / 2),
      point2D(-length, width / 2),
      point2D(0, 0)
    )

    closedPath(arrowShape, None, Some(color))
    .boundedColorisableGraphicalContent
    .transform(
      translation(headPosition)
        compose
      rotation(orientation)
    )
  }

  case class ExprConnectionGui(
    shape: Expression[Touchable], graphicalContent: Expression[ColorisableGraphicalContent], parametricCurve: Expression[org.workcraft.graphics.ParametricCurve])

  def makeConnectionTouchable(curve: org.workcraft.graphics.ParametricCurve, partial: PartialCurveInfo): TouchableC =
    TouchableC(new Touchable {
      override def hitTest(point: Point2D.Double) = {
        val nearestT = curve.nearestPointT(point)
        nearestT < partial.tEnd && nearestT > partial.tStart && (curve.pointOnCurve(nearestT).distance(point) < HitThreshold)
      }
      override def boundingBox = BoundingBox(curve.boundingBox).expand(HitThreshold*2, HitThreshold*2)
    }, curve.pointOnCurve(0.5))

  def getConnectionGui(properties: VisualCurveProperties, context: VisualConnectionContext, data: StaticVisualConnectionData) = {
    val curve = data match {
      case Polyline(cps) => PolylineGui.makeCurve(properties, context, cps)
      case Bezier(cp1, cp2) => BezierGui.makeCurve(context, cp1, cp2)
    }
    val curveInfo = buildConnectionCurveInfo(properties.arrow, context.c1.touchable, context.c2.touchable, curve)
    val visiblePath = curve.shape(curveInfo.tStart, curveInfo.tEnd)
    val touchable = makeConnectionTouchable(curve, curveInfo)

    val bcgc = 
      (properties.arrow.map(arrow => arrowHead(properties.color, 
        curveInfo.arrowHeadPosition,
        curveInfo.arrowOrientation,
        arrow.length,
        arrow.width)) ++
        properties.label.map (label => {
	  val p = curve.pointOnCurve(0.5)
	  val d = curve.derivativeAt(0.5)
	  val dd = curve.secondDerivativeAt(0.5)

	  val q = if (d.getX < 0)  (-d)  else d

	  val labelPosition = point2D(label.bounds.logical.getCenterX, 
						 if ((q cross dd) > -0.01) label.bounds.logical.getMaxY
						 else label.bounds.logical.getMinY)

	  val offset = p - labelPosition

	  val transform = translation(offset) compose rotation(q, labelPosition)

	  label.transform(transform)
	})).foldLeft(shape(visiblePath, Some(properties.stroke, properties.color), None).boundedColorisableGraphicalContent)(_ compose _)

    val gc = bcgc.cgc

    new ConnectionGui(touchable, gc, curve)
  }
}
