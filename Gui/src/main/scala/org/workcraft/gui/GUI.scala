package org.workcraft.gui
import scalaz.effects.IO
import scalaz.effects.IO._
import scalaz.Scalaz._
import javax.swing.ImageIcon
import java.awt.Color
import org.apache.batik.util.XMLResourceDescriptor
import org.apache.batik.dom.svg.SAXSVGDocumentFactory
import org.apache.batik.bridge.UserAgentAdapter
import org.apache.batik.bridge.BridgeContext
import org.apache.batik.bridge.GVTBuilder
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Window
import javax.swing.KeyStroke
import javax.swing.JMenuItem
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JButton

object GUI {
  def menuItem(text: String, mnemonic: Option[Char], accelerator: Option[KeyStroke], action: () => Unit) = {
    val result = new JMenuItem(text)
    mnemonic.foreach(result.setMnemonic(_))
    accelerator.foreach(result.setAccelerator(_))
    result.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) = {
        action()
      }
    })
    result
  }
  
  def button(text: String, action: () => Unit) = {
    val result = new JButton(text)
    result.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) = {
        action()
      }
    })
    result
  }

  def createIconFromImage(resourcePath: String): IO[Option[ImageIcon]] = {
    val res = ClassLoader.getSystemResource(resourcePath)
    if (res == null)
      None
    else
      Some(new ImageIcon(res))
  }.pure

  def createIconFromSVG(path: String, height: Int, width: Int, background: Color): IO[_ <: Either[Throwable, ImageIcon]] = {
    try {
      System.setProperty("org.apache.batik.warn_destination", "false")

      val parser = XMLResourceDescriptor.getXMLParserClassName()
      val f = new SAXSVGDocumentFactory(parser)

      val document = f.createDocument(ClassLoader.getSystemResource(path).toString())

      val userAgentAdapter = new UserAgentAdapter()
      val bridgeContext = new BridgeContext(userAgentAdapter)
      val builder = new GVTBuilder()

      val graphicsNode = builder.build(bridgeContext, document)

      val sizeY = bridgeContext.getDocumentSize().getHeight()
      val sizeX = bridgeContext.getDocumentSize().getWidth()

      val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

      val g2d = bufferedImage.getGraphics.asInstanceOf[Graphics2D]
      if (background != null) {
        g2d.setColor(background)
        g2d.fillRect(0, 0, width, height)
      }

      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

      val scaleX = (width - 1) / sizeX
      val scaleY = (height - 1) / sizeY
      val scale = Math.min(scaleX, scaleY)
      g2d.scale(scale, scale)
      g2d.translate(0.5, 0.5)

      graphicsNode.paint(g2d)
      g2d.dispose()
      Right(new ImageIcon(bufferedImage))
    } catch {
      case e => Left(e)
    }
  }.pure

  def centerToParent(frame: Window, parent: Window) = {
    val parentSize = parent.getSize
    val mySize = frame.getSize
    val q = parent.getLocationOnScreen
    frame.setLocation(((parentSize.width - mySize.width) / 2) + q.x, ((parentSize.height - mySize.height) / 2) + q.y)
  }

  def centerAndSizeToParent(frame: Window, parent: Window) = {
    val parentSize = parent.getSize
    frame.setSize(parentSize.width / 2, parentSize.height / 2)
    centerToParent(frame, parent)
  }
}