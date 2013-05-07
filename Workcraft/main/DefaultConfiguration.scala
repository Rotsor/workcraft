package org.workcraft

import org.workcraft.plugins.petri2.PetriNetModule
import org.workcraft.plugins.petrify.PetrifyModule
import org.workcraft.plugins.lola.LolaModule
import org.workcraft.plugins.fsm.FSMModule
import org.workcraft.plugins.dot.DotModule
import org.workcraft.services._
import java.io.File
import org.workcraft.gui._

import scalaz._
import Scalaz._

object DefaultConfiguration {
  val services = 
    List(new PetriNetModule, new PetrifyModule,
	 new FSMModule, new LolaModule, new DotModule).
      map(x => (x.serviceProvider : GlobalServiceProvider)).foldRight(mzero[GlobalServiceProvider])((a,b) => a |+| b) |+|
      (new GlobalServiceProvider {
        def implementation[T](s : Service[GlobalScope, T]) : T = s match {
          case DefaultLayouterService =>
            Some(org.workcraft.plugins.dot.DotLayoutTool.layouter)
          case s => s.monoid.zero
        }
      } : GlobalServiceProvider)


  def main (args: Array[String]) = {
    org.workcraft.gui.Main.main ("default configuration", services, args)
  }
}
