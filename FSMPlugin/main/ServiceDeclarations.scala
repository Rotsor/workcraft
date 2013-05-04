package org.workcraft.plugins.fsm

import org.workcraft.scala.IdUtils._
import org.workcraft.services.ModelService
import scalaz.effect._
import scalaz.effect.IO._

object Services {

  type PublicNfa = Nfa[String, String]

  object NfaService extends ModelService[IO[PublicNfa]]
}
