package ond
package resource

import scala.language.higherKinds

/**
 * A WIP attempt at a slightly less stack-hungry version of Session; it's still not stack-safe
 */
sealed abstract class SessionF[F[_], Source, Handle, A](implicit M: Monad[F], R: Resource[F, Source, Handle]) {
  import SessionF._

  /**
   * `run` evaluates the `SessionF` computation using the `Source`
   * in order to obtain a session `Handle`.
   */
  def run(s: Source): F[A] = {
    /** Interpreter for `SessionF` terms which recursively evaluates the computation */
    @annotation.tailrec
    def loop(handle: Handle, rt: SessionF[F, Source, Handle, A]): Either[F[SessionF[F, Source, Handle, A]], F[A]] = rt match {
      case Return(a) => Right(M.pure(a))
      case Lift(a) => Right(a)
      case Ask(continue) =>
        loop(handle, Return(handle) flatMap continue)
      case bind: Bind[F, Source, Handle, A, _] =>
        bind.session match {
          case Return(a) =>
            loop(handle, bind.fn(a))
          case Lift(fa) =>
            Left(fa.map(bind.fn))
          case Ask(continue) =>
            loop(handle, Ask(h => continue(h).flatMap(bind.fn)))
          case Bind(session, g) =>
            loop(handle, session flatMap { c => g(c).flatMap(bind.fn) })
        }
    }

    /**
     * Helper function allowing `loop` to be written as a tail-recursive function
     */
    def fold(h: Handle, either: Either[F[SessionF[F, Source, Handle, A]], F[A]]): F[A] =
      either.fold(
        fa => fa flatMap { r => fold(h, loop(h, r)) },
        identity[F[A]]
      )

    R.using(s, h => fold(h, loop(h, this)))
  }

  /** Continues the computation with the result of applying the function `f` to the inner value */
  def map[B](f: A => B): SessionF[F, Source, Handle, B] = flatMap(a => Return(f(a)))

  /** Continues the computation using another computation `f` which requires the inner value */
  def flatMap[B](f: A => SessionF[F, Source, Handle, B]): SessionF[F, Source, Handle, B] = Bind(this, f)
}

object SessionF {
  /** Term to encode returning a pure value of type `A` inside the computation. */
  private[ond] final case class Return[F[_]: Monad, Source, Handle, A](value: A)(implicit R: Resource[F, Source, Handle]) extends SessionF[F, Source, Handle, A]
  /** Term to encode returning a monadic value of type `F[A]` lifted into the computation */
  private[ond] final case class Lift[F[_]: Monad, Source, Handle, A](functor: F[A])(implicit R: Resource[F, Source, Handle]) extends SessionF[F, Source, Handle, A]
  /** Term to encode a continuation which requires the session's `Handle` */
  private[ond] final case class Ask[F[_]: Monad, Source, Handle, A](continue: Handle => SessionF[F, Source, Handle, A])(implicit R: Resource[F, Source, Handle]) extends SessionF[F, Source, Handle, A]
  /**
   * Term to encode `flatMap` operations as a nested data structure so that
   *  they can be re-associated to the right by the interpreter
   */
  private[ond] final case class Bind[F[_]: Monad, Source, Handle, A, B](session: SessionF[F, Source, Handle, B], fn: B => SessionF[F, Source, Handle, A])(implicit R: Resource[F, Source, Handle]) extends SessionF[F, Source, Handle, A]

  /**
   * The `SessionF` constructor lifts a function which requires an
   *  open session `Handle` to return an `F[A]` into the computation
   */
  def apply[F[_]: Monad, Source, Handle, A](op: Handle => F[A])(implicit R: Resource[F, Source, Handle]): SessionF[F, Source, Handle, A] =
    Ask(h => Lift(op(h)))
}
