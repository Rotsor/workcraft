package org.workcraft.gui
import javax.swing.JMenu
import javax.swing.JMenuItem
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import org.workcraft.services._
import GUI.menuItem
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.Component
import javax.swing.JPopupMenu
import java.awt.EventDispatchThread
import javax.swing.event.MenuListener
import javax.swing.event.MenuEvent
import javax.swing.JSeparator
import org.workcraft.services.ExporterService
import org.workcraft.services.ExportJob
import org.workcraft.services.Format
import org.workcraft.services.DefaultFormatService
import org.workcraft.scala.Expressions._
import scalaz.effect.IO
import scalaz._
import Scalaz._
import org.workcraft.gui.modeleditor.UndoService

class EditMenu(mainWindow: MainWindow) extends ReactiveMenu("Edit") {

  // Must be lazy because Scala allows to read uninitialized values
  lazy val items = mainWindow.editorInFocus >>= (_.flatMap(_.content.editor.implementation(UndoService)) match {
    case Some(undo) => {
      (undo.undo <**> undo.redo) ( (undo, redo) =>
      (undo match {
        case Some(action) => List(menuItem("Undo: " + action.description, Some('U'), Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, ActionEvent.CTRL_MASK)), action.action))
        case None => {
          val item = menuItem("Nothing to undo", None, None, ().pure[IO])
          item.setEnabled(false)
          List(item)
        }
      }) ++ 
      (redo match {
        case Some(action) => List(menuItem("Redo: " + action.description, Some('R'), Some(KeyStroke.getKeyStroke(KeyEvent.VK_Y, ActionEvent.CTRL_MASK)), action.action))
        case None => {
          val item = menuItem("Nothing to redo", None, None, ().pure[IO])
          item.setEnabled(false)
          List(item)
        }
      })
      )
    }
    case None => {
      constant(List())
    }
  })

  setMnemonic('E')
}
