package io.chrisdavenport.data

object Maybe {
  /** Union of **/

  /**
    * The Maybe type encapsulates an optional value. 
    * A value of type Maybe a either contains a value of 
    * type a (represented as MJust a), 
    * or it is empty (represented as MNothing). 
    * Using Maybe is a good way to deal with errors or exceptional cases without
    * resorting to drastic measures such as error.
    * 
    * The Maybe type is also a monad. 
    * It is a simple kind of error monad, 
    * where all errors are represented by Nothing.
    *
    * This corresponds to the Data.Maybe in Haskell.
    **/
  sealed trait Maybe[+A]
  private final case class MJust[A](a: A) extends Maybe[A]
  private final case object MNothing extends Maybe[Nothing]

  /** Constructors **/
  def just[A](a: A): Maybe[A] = MJust[A](a)
  def nothing[A]: Maybe[A] = MNothing
  def checkNull[A](a: A): Maybe[A] = if (a == null) MNothing else MJust(a)
  def checkNonFatal[A](a: => A): Maybe[A] = {
    import scala.util.control.NonFatal
    try { just(a) } 
    catch { case NonFatal(_) => nothing[A] }
  }

  
  /**
    * The maybe function takes a default value, a function, and a Maybe value.
    * If the Maybe value is MNothing, the function returns the default value. 
    * Otherwise, it applies the function to the value inside the MJust and 
    * returns the result.
    **/
  def maybe[B, A](b: B)(f: A => B)(ma: Maybe[A]): B = ma match {
    case MJust(a) => f(a)
    case MNothing => b
  }

  /**
    * The isJust function returns True iff its argument is of the form MJust _.
    **/
  def isMJust[A](ma : Maybe[A]): Boolean = 
    maybe[Boolean, A](false)(_ => true)(ma)

  /**
    * The isNothing function returns True iff its argument is MNothing.
    **/
  def isMNothing[A](ma: Maybe[A]): Boolean =
    maybe[Boolean, A](true)(_ => false)(ma)

  /**
    * The fromMaybe function takes a default value and and Maybe value. 
    * If the Maybe is MNothing, it returns the default values; otherwise,
    * it returns the value contained in the Maybe.
    **/
  def fromMaybe[A](a: A)(ma: Maybe[A]): A =
    maybe[A, A](a)(identity)(ma)
  
  /**
    * The listToMaybe function returns MNothing on an empty list or MJust a 
    * where a is the first element of the list.
    **/
  def listToMaybe[A](l: List[A]): Maybe[A] = l match {
    case x :: _ => Maybe.just(x)
    case Nil => Maybe.nothing[A]
  }
  
  /**
    * The maybeToList function returns an empty list when given Nothing or a 
    * singleton list when not given Nothing.
    **/
  def maybeToList[A](ma: Maybe[A]): List[A] = 
    maybe[List[A], A](List.empty)(a => List(a))(ma)

  /**
    * The optionToMaybe function returns MNothing On Empty and returns
    * MJust on Some
    **/
  def optionToMaybe[A](o: Option[A]): Maybe[A] = 
    o.fold(Maybe.nothing[A])(Maybe.just)

  /**
    * The maybeToOption function returns Empty on MNothing and returns
    * Some on MJust
    **/
  def maybeToOption[A](ma: Maybe[A]): Option[A] =
    maybe[Option[A], A](Option.empty[A])(Some(_))(ma)

  def fmap[A,B](f: A => B)(ma: Maybe[A]): Maybe[B] =
    maybe[Maybe[B], A](Maybe.nothing[B])(a => Maybe.just(f(a)))(ma)

  def bind[A, B](f: A => Maybe[B])(ma: Maybe[A]): Maybe[B] = 
    maybe[Maybe[B], A](Maybe.nothing[B])(f)(ma)
}