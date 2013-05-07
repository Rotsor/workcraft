package org.workcraft.gui.modeleditor.sim

import scalaz.effect.IO
import org.workcraft.scala.Expressions._

trait SimulationModel[Event, State] {
  def state: Expression[State]
  def enabled: Expression[Event => Boolean]
  
  def setState (state: State): IO[Unit]
  def fire(event : Event): IO[Unit]
  def name (e: Event) : String
}