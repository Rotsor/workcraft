package org.workcraft.plugins.petrify
import java.io.File
import org.workcraft.plugins.petri2.EditablePetriNet
import org.workcraft.plugins.petri2.PetriNetModel
import org.workcraft.plugins.petri2.PnFormatParser
import org.workcraft.plugins.petri2.VisualPetriNet
import org.workcraft.services.FileOpen
import org.workcraft.services.FileOpenJob
import org.workcraft.services.Format
import scalaz.effect.IO._
import scalaz.effect.IO
import scalaz.Scalaz._

object DotGFileOpen extends FileOpen {
  val description = "Workcraft .g format importer"
  val sourceFormat = Format.DotG

  def checkFile(file: File) = IO { file.getName().endsWith(".g") }

  override def open(file: File) = checkFile(file).map(
    if (_)
      Some(FileOpenJob(
      DotGParser.parseDotG(file) >>= {
        case Left(error) => IO { Left(error) }
        case Right(dotg) => PetriNetBuilder.buildPetriNet(dotg) >>= 
	  (pn => EditablePetriNet.create(VisualPetriNet.withDefaultLayout(pn)).map( epn => Right( new PetriNetModel (epn))))
      }))
    else
      None)
}
