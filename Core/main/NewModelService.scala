package org.workcraft.services

import scalaz.effect.IO

object NewModelService extends MultiService[GlobalScope, NewModelImpl]

trait NewModelImpl {
  def name: String
  def create: IO[Model]
}
