package org.workcraft.plugins.fsm
import org.workcraft.services.FileOpen
import java.io.File
import org.workcraft.services.FileOpenJob
import scalaz.effect.IO
import scalaz.effect.IO._
import org.workcraft.services.Format
import org.workcraft.services.ModelServiceProvider
import scalaz.Scalaz._

object FsmFileOpen extends FileOpen {

  val description = "Default Workcraft Petri Net format importer"
  val sourceFormat = Format.WorkcraftPetriNet

  def checkFile(file: File) = IO { file.getName().endsWith(".fsm") }

  override def open(file: File) = checkFile(file).map(
    if (_)
      Some(FileOpenJob(FsmFormatParser.parseFile(file).flatMap {case Left(error) => IO {Left(error)}; case Right(res) => 
          for(
            fsm <- FSM.create(res);
            efsm <- EditableFSM.create(VisualFSM.Minimal(fsm)))
          yield Right(FSMModel(efsm))}))
    else None)
}
