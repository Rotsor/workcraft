package org.workcraft.plugins.fsm

import org.workcraft.scala.IdUtils._
import org.workcraft.services.ModelService
import org.workcraft.scala.effects._

object Services {

  type PublicNfa = Nfa[String, String]


  object NfaService extends ModelService[IO[PublicNfa]]
}
