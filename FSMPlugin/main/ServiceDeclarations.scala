package org.workcraft.plugins.fsm

import org.workcraft.scala.IdUtils._
import org.workcraft.scala.effects._
import org.workcraft.services.ModelService

object Services {

  case class LabelledNfa[S,E] (
    dfa : Nfa[S, E],
    stateLabel : S => String,
    eventLabel : E => String
  )

  type PublicNfa = LabelledNfa[_,_]

  class NfaService extends ModelService[IO[PublicNfa]]
}
