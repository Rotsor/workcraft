package org.workcraft.plugins.petri.tools

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStream

import scalaz._
import Scalaz._

object Trace {
  def fromString(str : String) : Trace = Trace(str.split("\n").toList flatMap (_.trim.split(",").toList) map (_.trim))

  @throws(classOf[IOException])
  def save (os : OutputStream, trace : Trace) = os.write(trace.toString.getBytes)

  @throws(classOf[IOException])
  def load (is : InputStream, trace : Trace) : Trace = {
    Trace(new BufferedReader(new InputStreamReader(is)).readLine().split(",").toList)
  }
}

case class Trace(list : List[String]) {
  override def toString : String = list.mkString(",")
  def apply(i : Int) = list(i)
  def isEmpty = list.isEmpty
  def size = list.size
}


object TraceStep {
  import scalaz.Lens
  import Lens._
  val trace : Lens[TraceStep, Trace] = lensg(p => m => p.copy(trace = m), _.trace)
  val step : Lens[TraceStep, Int] = lensu((p, m) => p.copy(step = m), _.step)
}

case class TraceStep(trace : Trace, step : Int)
