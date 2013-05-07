package org.workcraft.plugins.petri2

import org.workcraft.scala.Expressions._
import scalaz.effect.IO._
import scalaz.effect.IO
import scalaz.Scalaz._

case class NameGenerator[T](names: Expression[Map[String, T]], prefix: String) {
  var counter = 0
  def newName =
    names.eval >>= (names => IO {
      def name = prefix + counter
      while (names.contains(name)) counter += 1
      val result = name
      counter += 1
      result
    })
}