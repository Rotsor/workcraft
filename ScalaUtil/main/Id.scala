package org.workcraft.scala

object IdUtils {
  /**
    * A class to be used as a unique identifier. 
    * It uses Java reference equality, but has a referentially-transparent interface.
    * forall x. (newId map (y => y == x)) == (newId map (y => false))
    */
  class Id[T] private[IdUtils] ()
  def newId[T] : effects.IO[Id[T]] = effects.IO.ioPure.pure { new Id }

  /**
    * A class to be used as a unique reference.
    * It uses Java reference equality, but has a referentially-transparent interface.
    * for all r1, r2: r1.id == r2.id => r1.get == r2.get
    */
  class Reference[T] private[IdUtils] (val id : Id[T], val get : T) {
    override def ==(ref : Reference[T]) = id == ref.id
    override def getHashCode = id.getHashCode
  }
  def newReference[T](t : T) = newId[T] map (id => new Reference(id, t))
}
