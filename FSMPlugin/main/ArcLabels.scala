package org.workcraft.plugins.fsm

import scalaz._
import Scalaz._

object ArcLabels {
  def printLabels : String => NonEmptyList[Option[String]] => String =
    sep => _.map { _.getOrElse ("ε") }.list.mkString(sep)
  def parseLabels : String => NonEmptyList[Option[String]] =
    s => s.split(",").map(_.trim).map{
      case "" => None
//      case "0" => None
      case "ε" => None
      case w => Some(w)
    }.toList match {
      case Nil => NonEmptyList.nel(None, Nil)
      case (x :: xs) => NonEmptyList.nel(x, xs)
    }
}
