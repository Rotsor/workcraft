package org.workcraft.plugins.petri2

import org.workcraft.scala.Expressions._
import org.workcraft.scala.Scalaz._
import org.workcraft.gui.CommonVisualSettings
import org.workcraft.graphics.Graphics._
import org.workcraft.graphics.Java2DDecoration._
import java.awt.Color
import java.awt.Font
import java.awt.geom.Point2D
import org.workcraft.graphics.BoundedColorisableGraphicalContent

object TokenPainter {
  def image(tokens: Int, settings: CommonVisualSettings): Option[BoundedColorisableGraphicalContent] = {
    val singleTokenSize = settings.size / 1.9;
    val multipleTokenSeparation = settings.strokeWidth / 8
    if (tokens == 0)
      None
    else if (tokens == 1)
      Some(circle(singleTokenSize, None, Some(settings.foregroundColor)).boundedColorisableGraphicalContent)
    else if (tokens > 1 && tokens < 8) {
      import math._
      val radialTokens = min(tokens, 6)
      val angularDistance = Pi / radialTokens
      val R = (settings.size / 2 - settings.strokeWidth - multipleTokenSeparation) / (1 + sin(angularDistance))
      val radiusTight = R * sin(angularDistance)
      val radiusSparse = radiusTight - multipleTokenSeparation

      val tokenImage = circle(radiusSparse * 2, None, Some(settings.foregroundColor)).boundedColorisableGraphicalContent

      val radialTokensImage =
        Range(0, radialTokens).map(i => tokenImage transform 
          (rotation(i * angularDistance * 2) compose translation(point(-R, 0))))
          .reduceLeft(_.compose(_))

      if (tokens == 7)
        Some(radialTokensImage.compose(tokenImage))
      else
        Some(radialTokensImage)
    } else
      Some(label(tokens.toString, settings.labelFont.deriveFont(settings.labelFontSize.toFloat),
        settings.foregroundColor).boundedColorisableGraphicalContent.centerToBoundingBox)
  }
}
