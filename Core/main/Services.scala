package org.workcraft

package object services {

import scalaz._
import Scalaz._

sealed trait Scope

trait GlobalScope extends Scope

trait ModelScope extends Scope

trait EditorScope extends Scope

// these must support identity comparison
trait Service[S <: Scope, ImplT] {
  implicit def monoid : Monoid[ImplT]
}

// these must support identity comparison
trait MultiService[S <: Scope, ImplT] extends Service[S, List[ImplT]] {
  override def monoid : Monoid[List[ImplT]] = scalaz.Monoid.monoid(
    Scalaz.semigroup((a,b) => a ++ b),
    Scalaz.zero(Nil)
  )
}

trait SingleService[S <: Scope, ImplT] extends Service[S, Option[ImplT]] {
  override def monoid : Monoid[Option[ImplT]] = scalaz.Monoid.monoid(
    Scalaz.semigroup(
      (a, b) => b match {
        case None => a
        case b => b
      }),
    Scalaz.zero(None))
}

trait ServiceProvider[S <: Scope] {
  def implementation[T](service: Service[S, T]): T
}

type GlobalServiceProvider = ServiceProvider[GlobalScope]

object ServiceProvider {
/*  def singleton[S <: Scope, T](service : Service[S, T], value : T) : ServiceProvider[S]
    = new ServiceProvider[S] {
      import service._
      def implementation[T2](s : Service[S,T2]) =
        if(s == service) value.asInstanceOf[T] else mzero
    }*/

  implicit def zero[S <: Scope] : Zero[ServiceProvider[S]] = Scalaz.zero(
    new ServiceProvider[S] {
      def implementation[T](service : Service[S, T]) : T = {
        service.monoid.zero
      }
    })
  implicit def semigroup[S <: Scope] : Semigroup[ServiceProvider[S]] = 
    Scalaz.semigroup((a,b) => {
      val e = new Exception
      new ServiceProvider[S]{
      def implementation[T](service : Service[S, T]) : T = {
        import service.monoid
        a.implementation(service) |+| b.implementation(service)
      }}
    })
}

type ModelServiceProvider = ServiceProvider[ModelScope]

type EditorServiceProvider = ServiceProvider[EditorScope]


type Model = ModelServiceProvider
type ModelService[T] = SingleService[ModelScope, T]
type GlobalService[T] = MultiService[GlobalScope, T]


}
