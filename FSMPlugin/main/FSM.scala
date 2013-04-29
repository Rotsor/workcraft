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

case class FSM(
  states: List[State], 
  arcs: List[Arc], 
  finalStates: Set[State], 
  initialState: State, 
  labels: Map[State, String], 
  arcLabels: Map[Arc, NonEmptyList[Option[String]]]) 
{
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
    def successors = 
      FSM.buildMap(for(
        arc <- arcs;
        label <- arcLabels(arc).list
      ) yield (labels(arc.from), (label, labels(arc.to))))
      .mapValues(_.list)
      .withDefault(_ => List())
    Nfa(
      labels(initialState),
      str => successors(str),
      str => finalStates.contains(from(str)),
      Some(states.toList.map(s => labels(s)))
    )
  }
}

object FSM {
  def Minimal = {
    val st = new State
    FSM(List(st), List(), Set(), st, Map(st -> "s0"), Map())
  }

  def buildMap[A,B] : List[(A,B)] => Map[A, NonEmptyList[B]] =
    l => l.groupBy(_._1).mapValues { 
      case (h :: t) => nel(h,t).map(_._2); 
      case Nil => error("groupBy is not supposed to return empty lists")
    }

  /**
    * Creates an editor-friendly FSM from the math-friendly Nfa.
    **/
  def create (nfa : Services.PublicNfa) : IO[FSM] = {
    def createAssocs[M[_],A,B](l : List[A])(create : A => M[B])(implicit monad : Applicative[M]) : M[(List[B],Map[A,B],Map[B,A])] =
      l.traverse(x => create(x) map (s => (x,s))).
        map(l => (l map (_._2), l.toMap, l.map(_.swap).toMap))

    for(
      sss <- createAssocs (nfa.allStates.toList) {_ => ioPure.pure{new State}};
      val (states, to, from) = sss;
      val arcLabels = buildMap(
          for(
            s1 <- nfa.allStates.toList;
            (e, s2) <- nfa.transitions(s1)
          ) yield ((s1, s2), e));
      aaa <- createAssocs(arcLabels.keys.toList){case (s1, s2) => ioPure.pure{new Arc(to(s1), to(s2))}};
      val (arcs, ato, afrom) = aaa;
      val initial = to(nfa.initial);
      val finals = to.toList.flatMap{case(s1,s2) => if (nfa.isFinal(s1)) List(s2) else List()}.toSet;
      val labels = from;
      val aLabels = arcLabels.map{case (k,v) => (ato(k), v) }
    )
    yield { FSM(states, arcs, finals, initial, labels, aLabels) }
  }
}

case class VisualFSM(fsm: FSM, layout: Map[State, Point2D.Double], visualArcs: Map[Arc, StaticVisualConnectionData])

object VisualFSM {
  def Minimal(fsm : FSM) = {
    import org.workcraft.dom.visual.connections._
    import java.awt.geom.Point2D
    VisualFSM(fsm, 
              fsm.states.map(s => (s, new Point2D.Double(0,0))).toMap,
              fsm.arcs.map(a => (a, Polyline(List()))).toMap)
  }
}
