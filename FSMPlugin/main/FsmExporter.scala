package org.workcraft.plugins.fsm

import org.workcraft.services.Exporter
import org.workcraft.services.Format
import org.workcraft.services.ModelServiceProvider
import org.workcraft.services.ServiceNotAvailableException
import org.workcraft.services.ExportJob
import java.io.OutputStream
import org.workcraft.scala.effects.IO
import org.workcraft.scala.effects.IO._
import java.io.PrintWriter
import java.io.BufferedOutputStream
import scalaz._
import Scalaz._
import java.io.FileOutputStream
import java.io.File
import org.workcraft.services.ExportError
import Services._

object FsmExporter extends Exporter {
  val targetFormat = Format.WorkcraftFsm

  def export(model: ModelServiceProvider): Either[ServiceNotAvailableException, ExportJob] = model.implementation(NfaService) match {
    case Some(snapshot) => Right(new ExportJob {
      def job(file: File) = snapshot >>= (net => FsmFormatParser.printFile(file, net.allData).map(_ => None))
    })
    case None => Left(new ServiceNotAvailableException(NfaService))
  }
}
