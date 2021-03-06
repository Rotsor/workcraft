
package org.workcraft.services
import java.io.File
import java.io.IOException
import java.io.InputStream
import org.workcraft.exceptions.DeserialisationException
import scalaz.effect.IO

trait Importer {
	def accept (file : File) : IO[Boolean]
	def description : String
	def importFrom (int : InputStream) : IO[Model] // will throw on error
}

object ImporterService extends MultiService[GlobalScope, Importer]
