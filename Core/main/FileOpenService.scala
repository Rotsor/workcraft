package org.workcraft.services
import java.io.File
import scalaz.effect.IO
import java.io.InputStream

object FileOpenService extends MultiService[GlobalScope, FileOpen]

trait FileOpen {
  val sourceFormat: Format
  val description: String
  def open (file: File): IO[Option[FileOpenJob]]
}

case class FileOpenJob (val job: IO[Either[String, ModelServiceProvider]])
