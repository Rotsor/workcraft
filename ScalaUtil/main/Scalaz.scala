package org.workcraft.scala

import scalaz.{Applicative}
import scalaz.Scalaz._

object Scalaz {
  class FA[F[_],A](x : F[A]) {
    def <|**|>[B,C](y : F[B], z : F[C])(implicit f : Applicative[F]) : F[(A, B, C)]
      = ^^(x, y, z)((a,b,c) => (a,b,c))
    def <|***|>[B,C,D](y : F[B], z : F[C], w : F[D])(implicit f : Applicative[F]) : F[(A, B, C, D)]
      = ^^^(x, y, z, w)((a,b,c,d) => (a,b,c,d))
  }
  implicit def qqqfa[F[_],A](x : F[A]) = new FA[F,A](x)
}
