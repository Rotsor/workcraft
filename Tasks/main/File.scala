package org.workcraft.tasks
import org.workcraft.scala.effects.IO
import org.workcraft.scala.effects.IO._

import java.io.File._

object File {

  def withTempFile[E,A](prefix : String, suffix : String,
    task : java.io.File => Task[A,E]) : Task[A,E] = new Task[A,E] {
    def runTask(tc : TaskControl) : IO[Either[Option[E], A]] = {
      wrapImpure{createTempFile(prefix, suffix)}.
        bracket(
          f => wrapImpure(f.delete)) (
          f => task(f).runTask(tc))
    }
  }

}
