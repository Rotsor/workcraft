package org.workcraft.gui

import org.workcraft.services._
import org.workcraft.services.layout._
import scalaz._
import Scalaz._
import org.workcraft.tasks._
import org.workcraft.gui.tasks.ModalTaskDialog

object Local {
  type AnyErrorTask[A] = Task[A,Any]
}

import Local._

object DefaultLayouterService extends 
    SingleService[GlobalScope, Layouter[AnyErrorTask]]

object DefaultLayout {
  def runDefaultLayout(
    global : GlobalServiceProvider
    , model : ModelServiceProvider
    , mainWindow : MainWindow) = {
    model.implementation(LayoutableService).traverse_(layoutable =>
      global.implementation(DefaultLayouterService).traverse_(layouter => 
        ModalTaskDialog.runTask(mainWindow, "Generating default layout", layoutable.apply(layouter)) 
          map {_ => ()}
      )
    )
  }
}
