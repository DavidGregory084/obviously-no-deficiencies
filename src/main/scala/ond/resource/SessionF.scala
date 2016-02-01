package ond
package resource

import scala.language.higherKinds

/**
 * A stack-safe version of Session
 */
sealed abstract class SessionF[F[_], Source, Handle, A](implicit M: Monad[F], R: Resource[F, Source, Handle]) {
  import SessionF._

  /** Interpreter for `SessionF` terms which recursively evaluates the computation */
  @annotation.tailrec
  protected def loop(h: Handle): F[A] =
    this match {
      case Return(a) => M.pure(a)
      case Ask(continue) => M.pure(continue(h))
      case bind: Bind[F, Source, Handle, A, _] =>
        bind.session match {
          case Return(a) =>
            bind.fn(a).loop(h)
          case ask @ Ask(continue) =>
            bind.fn(continue(h)).loop(h)
          case Bind(session, g) =>
            session.flatMap(c => g(c).flatMap(bind.fn)).loop(h)
        }
    }

  /**
   * `run` evaluates the `SessionF` computation using the `Source`
   * in order to obtain a session `Handle`.
   */
  def run(s: Source): F[A] =
    R.using(s, loop)

  /** Continues the computation with the result of applying the function `f` to the inner value */
  def map[B](f: A => B): SessionF[F, Source, Handle, B] = flatMap(a => Return(f(a)))

  /** Continues the computation using another computation `f` which requires the inner value */
  def flatMap[B](f: A => SessionF[F, Source, Handle, B]): SessionF[F, Source, Handle, B] = Bind(this, f)
}

object SessionF {
  /** Term to encode returning a pure value of type `A` inside the computation. */
  private[ond] final case class Return[F[_]: Monad, Source, Handle, A](value: A)(implicit R: Resource[F, Source, Handle]) extends SessionF[F, Source, Handle, A]
  /** Term to encode a continuation which requires the session's `Handle` */
  private[ond] final case class Ask[F[_]: Monad, Source, Handle, A](continue: Handle => A)(implicit R: Resource[F, Source, Handle]) extends SessionF[F, Source, Handle, A]
  /**
   * Term to encode `flatMap` operations as a nested data structure so that
   *  they can be re-associated to the right by the interpreter
   */
  private[ond] final case class Bind[F[_]: Monad, Source, Handle, A, B](session: SessionF[F, Source, Handle, B], fn: B => SessionF[F, Source, Handle, A])(implicit R: Resource[F, Source, Handle]) extends SessionF[F, Source, Handle, A]

  /**
   * The `SessionF` constructor lifts a function which requires an
   *  open session `Handle` to return an `A` into the computation
   */
  def apply[F[_]: Monad, Source, Handle, A](op: Handle => A)(implicit R: Resource[F, Source, Handle]): SessionF[F, Source, Handle, A] =
    Ask(op)
}
