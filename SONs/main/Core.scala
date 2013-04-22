package org.workcraft.sons

import scalaz._
import Scalaz._

package object core {

  type Node[S,E] = Either[S,E]

// states[s] represents pre- and post-set of the state s
// the graph must be acyclic
  case class OccurrenceNet[S,E](
    states : Map[S,(Option[E], Option[E])]
  ) {
    def nodes : List[Node[S,E]] =
      states.keys.toList.map(Left(_:S)) ++ ((states.values >>= {case (l,r) => l.toList ++ r.toList}).toSet.toList.map(Right(_:E)))
  }
  object OccurrenceNet {
    def Empty[S,E] = OccurrenceNet(Map[S,(Option[E],Option[E])]())
  }

  import org.workcraft.graphics.Java2DDecoration._

  type VisualOccurrenceNet[S,E] = (OccurrenceNet[S,E], Node[S,E] => Point)
}
