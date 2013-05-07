package org.workcraft.tasks
import scalaz.effect.IO
import scalaz.effect.IO._

import java.io.File._

object File {

  def withTempFile[E,A](prefix : String, suffix : String,
    task : java.io.File => Task[A,E]) : Task[A,E] = new Task[A,E] {
    def runTask(tc : TaskControl) : IO[Either[Option[E], A]] = {
      IO{createTempFile(prefix, suffix)}.
        bracket(
          f => IO(f.delete)) (
          f => task(f).runTask(tc))
    }
  }

}
