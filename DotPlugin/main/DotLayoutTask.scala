package org.workcraft.plugins.dot
import java.io.File
import org.workcraft.tasks.Task
import org.workcraft.tasks.TaskControl
import scalaz.effect.IO._
import scalaz.effect.IO
import scalaz.Scalaz._
import org.workcraft.tasks.ExternalProcess

class DotLayoutTask(dotCommand: String, input: File, output: File) extends Task[File, DotError] {
  def runTask(tc: TaskControl) =
    tc.descriptionUpdate("Running dot...") >>
      ExternalProcess.runSyncCollectOutput(List(dotCommand, input.getAbsolutePath, "-Tdot", "-o", output.getAbsolutePath), None, tc.cancelRequest) >>= {
        case Left(cause) => IO { Left(Some(DotError.CouldNotStart(cause))) }
        case Right((exitValue, cancelled, stdout, stderr)) => IO {
          if (cancelled) Left(None) else exitValue match {
            case 0 => Right(output)
            case _ => Left(Some(DotError.RuntimeError(new String(stderr, "US-ASCII"))))
          }
        }
      }
}
