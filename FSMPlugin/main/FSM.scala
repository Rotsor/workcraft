package org.workcraft.plugins.fsm
import java.awt.geom.Point2D
import org.workcraft.dom.visual.connections.StaticVisualConnectionData
import scalaz._
import scalaz.Scalaz._
import org.workcraft.scala.effects._
import org.workcraft.scala.effects.IO._

sealed trait Node

class State extends Node
class Arc(val from: State, val to: State) extends Node

case class FSM(states: List[State], arcs: List[Arc], finalStates: Set[State], initialState: State, labels: Map[State, String], arcLabels: Map[Arc, String]) {
  lazy val names = labels.map(_.swap).toMap

  lazy val postset: Map[State, List[(State, Arc)]] =
    arcs.foldLeft((Map[State, List[(State, Arc)]]().withDefault(_ => List()))) {
      case (map, arc) => (map + (arc.from -> ((arc.to, arc) :: map(arc.from))))
    }

  lazy val preset: Map[State, List[(State, Arc)]] =
    arcs.foldLeft((Map[State, List[(State, Arc)]]().withDefault(_ => List()))) {
      case (map, arc) => (map + (arc.to -> ((arc.from, arc) :: map(arc.to))))
    }
  def toNfa : Nfa[String, String] = {
    def from : Map[String, State] = states.toList.map(s => (labels(s), s)).toMap
    def successors = arcs.map(arc => (labels(arc.from), (arcLabels.get(arc) >>= {case ""=>None; case s => Some(s)}, labels(arc.to)))).groupBy(_._1).mapValues(_.map(_._2)).withDefault(_ => List())
    Nfa(labels(initialState), str => successors(str)
      , str => finalStates.contains(from(str)))
  }
}

object FSM {
  def Minimal = {
    val st = new State
    FSM(List(st), List(), Set(), st, Map(st -> "s0"), Map())
  }

  /**
    * Creates an editor-friendly FSM from the math-friendly Nfa.
    **/
  def create (nfa : Services.PublicNfa) : IO[FSM] = {
    def createAssocs[M[_],A,B](create : A => M[B])(l : List[A])(implicit monad : Applicative[M]) : M[(List[B],Map[A,B],Map[B,A])] =
      l.traverse(x => create(x) map (s => (x,s))).
        map(l => (l map (_._2), l.toMap, l.map(_.swap).toMap))

    for(
      sss <- createAssocs{(_ : String) => ioPure.pure{new State}}(nfa.allStates.toList);
      val (states, to, from) = sss;
      aaa <- createAssocs[IO,((String, (Option[String], String)),Int),Arc]{case ((s1, (e, s2)),_) => ioPure.pure{new Arc(to(s1), to(s2))}}(nfa.allStates.toList.flatMap(s => nfa.transitions(s).map(t => (s,t))).zipWithIndex);
      val (arcs, ato, afrom) = aaa;
      val initial = to(nfa.initial);
      val finals = to.toList.flatMap{case(s1,s2) => if (nfa.isFinal(s1)) List(s2) else List()}.toSet;
      val labels = from;
      val aLabels = ato.flatMap[(Arc,String), Map[Arc, String]]{case (((_, (None,_)), _),_) => Map(); case(((_,(Some(x), _)),_), a)=>Map((a, x)) }
    )
    yield { FSM(states, arcs, finals, initial, labels, aLabels) }
  }
}

case class VisualFSM(fsm: FSM, layout: Map[State, Point2D.Double], visualArcs: Map[Arc, StaticVisualConnectionData])

object VisualFSM {
  def Minimal = {
    val fsm = FSM.Minimal
    VisualFSM(fsm, Map(fsm.states.head -> new Point2D.Double(0, 0)), Map())
  }
}
