package org.workcraft.scala

import org.workcraft.dependencymanager.advanced.user.{ModifiableExpression => JModifiableExpression, Variable}
import scalaz._
import scala.collection.generic.CanBuildFrom
import scala.collection.TraversableLike
import org.workcraft.dependencymanager.advanced.core.{Expression => JExpression}
import org.workcraft.dependencymanager.advanced.core.Expressions.{constant => jConstant, modifiableExpression => jModifiableExpression, joinCollection => joinCollectionJ, bind => bindJ, fmap => fmapJ}
import org.workcraft.dependencymanager.advanced.core.GlobalCache
import scala.collection.JavaConversions.{asJavaCollection, asScalaIterable}
import Expressions._
import org.workcraft.scala.Scalaz._
import org.workcraft.scala.Util._
import org.workcraft.dependencymanager.advanced.user.Setter
import javax.swing.SwingUtilities
import org.workcraft.scala.effects.IO
import org.workcraft.scala.effects.IO._

object Expressions {
  case class Expression[+T](jexpr : JExpression[_ <: T])
   
  case class ModifiableExpression[T](expr : Expression[T], val setValue : T => Unit) {
    def jexpr : JModifiableExpression[T] = {
      def sv(t : T) = setValue(t)
      jModifiableExpression((this : Expression[T]).jexpr, new Setter[T]{
      override def setValue(t : T) {
        sv(t)
      }
    })
    }
  }
  
  class ThreadSafeVariable[T](initialValue: T) extends Variable[T](initialValue) {
    override def setValue(value: T) = {
      SwingUtilities.invokeAndWait(new Runnable(){
        def run = ThreadSafeVariable.super.setValue(value)
      })
    }
  }
  
  def expr[A, T <: Expression[A]](a: T): Expression[A] = a
  
  implicit def convertModifiableExpression[T](me : JModifiableExpression[T]) = ModifiableExpression[T](decorateExpression(me), x => me.setValue(x))
  
  implicit object JExpressionMonad extends Monad[JExpression] {
    override def pure[A] (x : => A) = jConstant(x)
    override def bind[A,B](a : JExpression[A], f : A => JExpression[B]) : JExpression[B] = bindJ(a, asFunctionObject(f))
  }
  
  implicit def decorateExpression[T](e : JExpression[_ <: T]): Expression[T] = Expression[T](e)
  
  implicit def modifiableExpressionAsReadonly[T](m : ModifiableExpression[T]) : Expression[T] = m.expr
  
  implicit object ExpressionMonad extends Monad[Expression] {
    override def pure[A] (x : => A) = constant(x)
    override def bind[A,B](a : Expression[A], f : A => Expression[B]) : Expression[B] = decorateExpression(bindJ[A,B](a.jexpr : JExpression[_ <: A], asFunctionObject(((_ : Expression[B]).jexpr).compose(f))))
  }
  
  def constant[T](x : T) : Expression[T] = jConstant(x)
  
  /**
   *  Needed because Scala is stupid!
   */
  implicit def monadicSyntaxME[A](m: ModifiableExpression[A]) = new {
    def map[B](f: A => B) = implicitly[Monad[Expression]].fmap(m, f)
    def flatMap[B](f: A => Expression[B]) = implicitly[Monad[Expression]].bind(m, f)
    def >>=[B](f: A => Expression[B]) = implicitly[Monad[Expression]].bind(m, f)
  }
  
  implicit def monadicSyntaxV[A](m: Variable[A]) = monadicSyntaxME(m)
  implicit def monadicSyntaxJ[A](m: JModifiableExpression[A]) = monadicSyntaxME(m)
  
  trait ExpressionOps[+A] {
    def mapE[B](f : A => Expression[_ <: B]) : Expression[List[B]]
  }
  
  implicit def augmentWithExpressionOps[A, Coll <: Iterable[A]](coll : Coll) : ExpressionOps[A] = {
    new ExpressionOps[A]() {
      def mapE[B](f : A => Expression[_ <: B]) = {
        sequenceExpressions[B](coll.map(f))
      }
    }
  }
  
  implicit def implicitJexpr[T](e : Expression[T]) : JExpression[_ <: T] = e.jexpr
  implicit def implicitMJexpr[T](e : ModifiableExpression[T]) : JModifiableExpression[T] = e.jexpr
  
  def sequenceExpressions[A](collection : Iterable[Expression[_ <: A]]) : Expression[List[A]] = collection.foldRight(constant(Nil : List[A]))((head : Expression[_ <: A], tail : Expression[List[A]]) => for(tail <- tail; head <- head) yield head::tail)
  
  implicit def decorateModifiableExpression[T](me : ModifiableExpression[T]) = new {
    def modify(f : T => T) {
      me.setValue(f(unsafeEval(me.jexpr)))
    }
  }
  
  def unsafeEval[T](e : Expression[T]) : T = GlobalCache.eval(e.jexpr)

  def unsafeAssign[T](to : ModifiableExpression[T], from : Expression[T]) = GlobalCache.assign(to, from.jexpr)

  def eval[T](e: Expression[T]): IO[T] =  ioPure.pure {unsafeEval(e)}
  
  def update[T](e: ModifiableExpression[T])(f: T => T) = eval(e) >>= ( v => set (e, f(v)))
  
  def set[T](e: ModifiableExpression[T], value: T) = ioPure.pure { e.setValue(value) }
  
  def assign[T](to : ModifiableExpression[T], from : Expression[T]) : IO[Unit] = ioPure.pure (unsafeAssign(to, from))
  
  def newVar[T](x : T) : IO[Variable[T]] = ioPure.pure(Variable.create(x))
}
