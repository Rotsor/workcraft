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
  override def monoid : Monoid[List[ImplT]] = scalaz.std.list.listMonoid
}

trait SingleService[S <: Scope, ImplT] extends Service[S, Option[ImplT]] {
  override def monoid : Monoid[Option[ImplT]] = scalaz.Monoid.instance(
      (a, b) => b match {
        case None => a
        case b => b
      },None)
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

  implicit def monoid[S <: Scope] : Monoid[ServiceProvider[S]] = scalaz.Monoid.instance(
    (a,b) => {
      val e = new Exception
      new ServiceProvider[S]{
      def implementation[T](service : Service[S, T]) : T = {
        import service.{monoid => mmm}
        import scalaz.syntax.semigroup
        ToSemigroupOps(a.implementation(service)) |+| b.implementation(service)
      }}
    }, new ServiceProvider[S] {
      def implementation[T](service : Service[S, T]) : T = {
        service.monoid.zero
      }
    })
}

type ModelServiceProvider = ServiceProvider[ModelScope]

type EditorServiceProvider = ServiceProvider[EditorScope]


type Model = ModelServiceProvider
type ModelService[T] = SingleService[ModelScope, T]
type GlobalService[T] = MultiService[GlobalScope, T]


}
