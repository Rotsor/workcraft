package org.workcraft.scala

package effects

import scalaz._

trait IOMonad[M[_]] extends Monad[M] {
  def lift[A](x : IO[A]) : M[A]
}
