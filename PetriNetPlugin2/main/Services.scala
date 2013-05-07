package org.workcraft.plugins.petri2

import org.workcraft.services.ModelService
import scalaz.effect.IO

object PetriNetService extends ModelService[IO[PetriNet]]
object VisualPetriNetService extends ModelService[IO[VisualPetriNet]]
