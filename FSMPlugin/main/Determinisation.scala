package org.workcraft.plugins.fsm

object Utils {
  import scala.collection.mutable. { Set => MSet, Map => MMap }
  def unions[A] : List[Set[A]] => Set[A] =
    x => x.foldLeft(Set[A]())((a,b) => a.union(b))
  def collect[T](x : T)(f : T => List[T]) : Set[T] = {
    var visited : MSet[T] = MSet()
    def go(s : T) : Unit = {
      if(visited.contains(s))
      {}
      else
      {
        visited += s
        f(s).map(go(_))
      }
    }
    go(x)
    visited.toSet
  }
}

case class Nfa[State, Symbol] (
  initial : State,
  transitions : State => List[(Option[Symbol], State)],
  isFinal : State => Boolean) {
  import Utils._
  def allStates = collect(initial)(s => transitions(s).map(_._2))
  def mapStates[State2](to : State => State2, from : State2 => State) = {
    Nfa[State2, Symbol](
      to(initial),
      s2 => transitions(from(s2)).map{case (k,v)=>(k,to(v))},
      s2 => isFinal(from(s2))
    )
  }
}

case class Dfa[State, Symbol] (
  initial : State,
  transitions : State => Map[Symbol, State],
  isFinal : State => Boolean
) {
  import Utils._
  def allStates = collect(initial)(s => transitions(s).toList.map(_._2))
  def allData = allStates.toList.map(s => (s, transitions(s), isFinal(s)))
  def nfa = Nfa[State,Symbol](initial, s => transitions(s).toList.map{case (sym,st) => (Some(sym), st)}, isFinal)
}

object Determinisation {
  import scala.collection.mutable. { Set => MSet, Map => MMap }
  def determinise [S,E] : Nfa[S,E] => Dfa[Set[S], E] = {

    case Nfa (initial, transitions, isFinal) => {
      type StateP = Set[S]
      import Utils._
      def epsReachable(s : S) = collect(s)(s => transitions(s).filter(_._1.isEmpty).map(_._2))

      var known : MMap[StateP, Map[E, StateP]] = MMap()
      val initP = epsReachable(initial)
      def go(states : StateP) : Unit = {
        if(known.contains(states)) {}
        else {
        val children : Map[E, StateP] =
          states.
            toList.
            map(s => transitions(s)).
            flatten.
            flatMap({case (None, _) => List(); case (Some(sym), s) => List((sym, s))}).
            groupBy(_._1).
            mapValues(ss => unions(ss.map(x => epsReachable(x._2))))
        known += ((states, children))
        children.mapValues(ss => go(ss)).toList
        }
      }
      go(initP)
      Dfa(initP, known(_), _.toList.map(s => isFinal(s)).foldLeft(false)(_||_))
    }
  }
}
