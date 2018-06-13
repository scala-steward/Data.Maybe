package io.chrisdavenport.data

package object maybe {
  def maybe[B, A](b: B)(f: A => B)(ma: Maybe[A]): B = 
    Maybe.fold(b)(f)(ma)

  def isJust[A](ma : Maybe[A]): Boolean = 
    maybe[Boolean, A](false)(_ => true)(ma)

  def isNothing[A](ma: Maybe[A]): Boolean =
    maybe[Boolean, A](true)(_ => false)(ma)

  def fromMaybe[A](a: A)(ma: Maybe[A]): A =
    maybe[A, A](a)(identity)(ma)
  
  def listToMaybe[A](l: List[A]): Maybe[A] = l match {
    case x :: _ => Maybe.just(x)
    case Nil => Maybe.nothing[A]
  }
  
  def maybeToList[A](ma: Maybe[A]): List[A] = 
    maybe[List[A], A](List.empty)(a => List(a))(ma)
}