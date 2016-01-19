package ond
package resource

import scala.language.higherKinds

final case class Session[F[_], Source, Handle, A](session: Handle => F[A])(implicit M: Monad[F], R: Resource[F, Source, Handle]) {
  def map[B](f: A => B): Session[F, Source, Handle, B] =
    Session(h => session(h) map f)

  def flatMap[B](f: A => Session[F, Source, Handle, B]): Session[F, Source, Handle, B] =
    Session(h => session(h) flatMap { f(_).session(h) })

  def run(s: Source) = R.using(s, session)
}

