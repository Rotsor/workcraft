package org.workcraft.gui
import javax.swing.JMenu
import org.workcraft.scala.Expressions._
import scalaz.effect.IO._
import scalaz.effect.IO
import javax.swing.JMenuItem
import javax.swing.event.MenuEvent
import javax.swing.event.MenuListener
import javax.swing.JComponent

abstract class ReactiveMenu(title: String) extends JMenu(title) {
  def items: Expression[List[JComponent]]
  
  val refresh = swingAutoRefresh(items, (i: List[JComponent]) => IO{
      removeAll()
      items.eval.unsafePerformIO.foreach(add(_))
      if (getItemCount() == 0) 
        setEnabled(false)
      else
        setEnabled(true)
  })
}
