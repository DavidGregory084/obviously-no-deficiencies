package ond
package resource

import scala.language.higherKinds

final case class Session[F[_], Source, Handle, A](session: Handle => A)(implicit M: Monad[F], R: Resource[F, Source, Handle]) {
  def map[B](f: A => B): Session[F, Source, Handle, B] =
    Session(h => f(session(h)))

  def flatMap[B](f: A => Session[F, Source, Handle, B]): Session[F, Source, Handle, B] =
    Session(h => f(session(h)).session(h))

  def run(s: Source) = R.using(s, session andThen M.pure)
}

