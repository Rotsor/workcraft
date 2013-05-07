package org.workcraft.tasks
import org.workcraft.scala.Expressions.ThreadSafeVariable
import org.workcraft.dependencymanager.advanced.core.GlobalCache
import org.workcraft.dependencymanager.advanced.core.Expression
import scalaz._
import Scalaz._
import scalaz.effect.IO
import scalaz.effect.MonadIO
import scalaz.effect.IO._

case class TaskControl(cancelRequest : IO[Boolean], progressUpdate: Double => IO[Unit], descriptionUpdate: String => IO[Unit])

trait Task[+O, +E] {
  import Task._

  def runTask (tc : TaskControl) : IO[Either[Option[E], O]]

  def flatMap[O2, E2 >: E](f: O => Task[O2, E2]) = {
    val outer = this
    
    new Task[O2, E2] {
      def runTask (tc: TaskControl) = outer.runTask(tc) >>= {
        case Left(error) => IO { Left(error) }
        case Right(output) => tc.cancelRequest.>>=[Either[Option[E2], O2]] {
          case true => IO { Left(None) }
          case false => f(output).runTask(tc)
        }
      }
    }
  }

  def map[O2](f : O => O2) = {
    def outer = this
    new Task[O2, E] {
      def runTask(tc : TaskControl) = outer.runTask(tc).map{
        case Left(error) => Left(error)
        case Right(result) => Right(f(result))
      }
    }
  }

  def >>= [O2, E2 >: E](f: O => Task[O2, E2]) = flatMap(f)
  
  def >> [O2, E2 >: E] (t: Task[O2, E2]) = flatMap(_ => t)
  def *> [O2, E2 >: E] (t: Task[O2, E2]) = >> (t)
  
  def mapError[E2] (f: E => E2) = {
    val outer = this
    new Task[O, E2] {
      def runTask (tc: TaskControl) = outer.runTask(tc) map {
        case Left(None) => Left(None)
        case Left(Some(error)) => Left(Some(f(error)))
        case Right(output) => Right(output)
      }
    }
  }

  def runAsynchronously (tc: TaskControl, finished: Either[Option[E], O] => IO[Unit]) : IO[Unit] = IO {
    new Thread() {
      override def run() = (runTask(tc) >>= finished).unsafePerformIO
    }.start()
  }
}

object Task {
  def pure[O, E](outcome: O) = new Task[O, E] {
    def runTask(tc: TaskControl) = Right(outcome).pure[IO]
  }

  def apply[E,O] (task: IO[Either[E, O]]) = new Task[O,E] {
    def runTask(tc: TaskControl) = task map {
      case Left(error) => Left(Some(error))
      case Right(value) => Right(value)
    }
  }
  
  def apply[E,O] (task: TaskControl => IO[Either[Option[E],O]]) = new Task[O,E] {
    def runTask(tc: TaskControl) = task(tc)
  }
  
   def ioTask[O,E] (action: IO[O]) = new Task[O, E] {
    def runTask(tc: TaskControl) = action >>= (x => Right(x).pure[IO])
  }

  implicit def ioMonad[E] = new MonadIO[({ type t[a] = Task[a , E] })#t] {
    def bind[O1, O2](a: Task[O1, E])(f: O1 => Task[O2, E]) = a.flatMap(f)
    def point[O](a: => O) = Task.pure(a)
    def liftIO[A](x : IO[A]) = ioTask(x)
  }

/*  implicit def ioMonadUnapply[O, E](t: Task[O, E]): Unapply[Monad, Task[O, E]] = new Unapply[Monad, Task[O,E]] {
    type A = O
    type M[X] = Task[X , E]
    def TC = ioMonad[E]
    def leibniz = implicitly
  }
  implicit def ioFunctorUnapply[O, E](t: Task[O, E]): Unapply[Functor, Task[O, E]] = new Unapply[Functor, Task[O,E]] {
    type A = O
    type M[X] = Task[X , E]
    def TC = ioMonad[E]
    def leibniz = implicitly
  } */

}
