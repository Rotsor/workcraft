package org.workcraft.swing

import scalaz.Monad

import scalaz.effect.IO
import scalaz.effect.IO._

class Swing[A] (run : IO[A]) {
  // assumes that it will be executed on the Swing thread.
  def unsafeRun : IO[A] = run
  def unsafePerformIO = unsafeRun.unsafePerformIO
}

object Swing {
  
  class SwingRef[A] private[swing] (iv : A) {
    var cv = iv
    def read : Swing[A] = liftIO(IO(cv))
    def write(v : A) : Swing[Unit] = liftIO(IO(cv = v))
    def mod(f: A => A) : Swing[Unit] = liftIO(IO{cv = f(cv)})
  }

  def newRef[A] (a : A) : Swing[SwingRef[A]] = liftIO(IO(new SwingRef(a)))
  

  def liftIO[A](run : IO[A]) = new Swing(run)
  def unsafeToSwing[A](run : => A) = liftIO(IO(run))
  
  implicit def swingMonad : Monad[Swing] = new Monad[Swing] {
    override def bind[A,B](x : Swing[A])(f : A => Swing[B]) : Swing[B] = liftIO(x.unsafeRun flatMap (a => f(a).unsafeRun))
    override def point[A](x : => A) : Swing[A] = liftIO(IO(x))
  }
}
