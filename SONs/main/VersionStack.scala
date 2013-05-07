package org.workcraft.sons

case class VersionStack[A](
  previous : List[(A, String)], 
  current : (A, String),
  next : List[(A, String)]) {
  def undo : Option[VersionStack[A]] = previous match {
    case h :: t => Some(VersionStack(t, h, current :: next))
    case _ => None
  }
  def redo : Option[VersionStack[A]] = next match {
    case h :: t => Some(VersionStack(current :: previous, h, t))
    case _ => None
  }
  def record(x : A, message : String) = VersionStack(current :: previous, (x, message), Nil)
}

object VersionStack {
  def initial[A] (x : A) : VersionStack[A] = VersionStack(Nil, (x, "Initial version"), Nil)
}
