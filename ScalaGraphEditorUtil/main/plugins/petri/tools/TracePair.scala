package org.workcraft.plugins.petri.tools

object TracePair {
  import scalaz.Lens
  def createEmpty = TracePair(TraceStep(Trace(Nil),0),TraceStep(Trace(Nil),0))
  val main : Lens[TracePair, TraceStep] = Lens.lensu((p, m) => p.copy(main = m), p => p.main)
  val branch : Lens[TracePair, TraceStep] = Lens.lensu((p, m) => p.copy(branch = m), p => p.branch)
}

case class TracePair(val main : TraceStep, val branch : TraceStep)

