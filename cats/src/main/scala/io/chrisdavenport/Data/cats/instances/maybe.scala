package io.chrisdavenport.Data.cats.instances

import cats._
import io.chrisdavenport.Data.{Maybe => M}
import io.chrisdavenport.Data.Maybe.Maybe

object maybe {
  implicit val catsStdInstancesForOption: Traverse[Maybe] with MonadError[Maybe, Unit] with Alternative[Maybe] with CommutativeMonad[Maybe] with CoflatMap[Maybe] =
    new Traverse[Maybe] 
      with MonadError[Maybe, Unit]
      with Alternative[Maybe]
      with CommutativeMonad[Maybe]
      with CoflatMap[Maybe] {
      // Members declared in cats.Applicative
      def pure[A](x: A): Maybe[A] = M.just(x)
      
      // Members declared in cats.ApplicativeError
      def handleErrorWith[A](fa: Maybe[A])(f: Unit => Maybe[A]): Maybe[A] =
        M.maybe[Maybe[A], A](f(()))(M.just(_))(fa)
      def raiseError[A](e: Unit): Maybe[A] = M.nothing[A]
      
      // Members declared in cats.CoflatMap
      def coflatMap[A, B](fa: Maybe[A])(f: Maybe[A] => B): Maybe[B] = pure(f(fa))
      
      // Members declared in cats.FlatMap
      def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] = M.>>=(fa)(f)

      def tailRecM[A, B](a: A)(f: A => Maybe[Either[A,B]]): Maybe[B] = {
        def eMap(e: Either[A, B]): Maybe[B] = e match {
          case Left(a) => tailRecM(a)(f)
          case Right(b) => M.just(b)
        }
        M.maybe(M.nothing[B])(eMap)(f(a))
      }
      
      // Members declared in cats.Foldable
      def foldLeft[A, B](fa: Maybe[A],b: B)(f: (B, A) => B): B = 
        M.maybe[B, A](b)(f(b, _))(fa)
      def foldRight[A, B](fa: Maybe[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
        M.maybe[Eval[B], A](lb)(f(_, lb))(fa)
      
      // Members declared in cats.MonoidK
      def empty[A]: Maybe[A] = M.nothing[A]
      
      // Members declared in cats.SemigroupK
      def combineK[A](x: Maybe[A],y: Maybe[A]): Maybe[A] = M.maybe[Maybe[A], A](y)(M.just(_))(x)
      
      // Members declared in cats.Traverse
      def traverse[G[_], A, B](fa: Maybe[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Maybe[B]] = 
        M.maybe[G[Maybe[B]], A](Applicative[G].pure(M.nothing[B]))(a => Applicative[G].map(f(a))(M.just(_)))(fa)

    }
}