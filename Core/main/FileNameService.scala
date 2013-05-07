package org.workcraft.services

import scalaz.effect.IO
import scalaz.effect.IO._

import scalaz._
import Scalaz._

object FileNameService extends SingleService[ModelScope, FileName] {
  def create (fileName: Option[String]) = new ModelServiceProvider {
    def implementation[T](service: Service[ModelScope,T]): T = service match {
      case FileNameService => Some (new Object with FileName {
	var name = fileName
	def lastSavedAs = name
	def update (savedAs: String) = IO { name = Some(savedAs) }
      })
      case s => s.monoid.zero
    }
  }
}

trait FileName {
  def lastSavedAs: Option[String]
  def update (savedAs: String): IO[Unit]
}
