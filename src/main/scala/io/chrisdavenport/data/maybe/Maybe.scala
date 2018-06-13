package io.chrisdavenport.data.maybe

sealed trait Maybe[+A]
object Maybe {
  def just[A](a: A): Maybe[A] = MJust[A](a)
  def nothing[A]: Maybe[A] = MNothing

  def maybeNull[A](a: A): Maybe[A] = if (a == null) MNothing else MJust(a)

  private final case class MJust[A](a: A) extends Maybe[A]
  private final case object MNothing extends Maybe[Nothing]

  private[maybe] def fold[B, A](b: B)(f: A => B)(ma: Maybe[A]): B = ma match {
    case MJust(a) => f(a)
    case MNothing => b
  }
}