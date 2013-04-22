package org.workcraft.sons

import org.workcraft.services.Module
import org.workcraft.services.Service
import org.workcraft.services.GlobalServiceProvider
import org.workcraft.services.NewModelImpl
import org.workcraft.services.GlobalScope
import org.workcraft.services.NewModelService
import org.workcraft.services.ModelServiceProvider
import org.workcraft.services.Exporter
import org.workcraft.services.ExporterService
import org.workcraft.services.FileOpenService

import scalaz._
import Scalaz._

import core._

object NewOccurrenceNet extends NewModelImpl {
  def name = "Occurrence Net"
  def create = EditableOccurrenceNet.create(OccurrenceNet.Empty)
}

object SonsServiceProvider extends GlobalServiceProvider {
  def implementation[T](service: Service[GlobalScope, T]) = service match {
    case NewModelService => List(NewOccurrenceNet)
    case s => import s._; mzero
  }
}

class SonsModule extends Module {
  def name = "Structured Occurrence Nets"
  def serviceProvider = SonsServiceProvider
}
