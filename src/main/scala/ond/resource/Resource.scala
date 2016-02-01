package ond
package resource

import scala.language.higherKinds

abstract class Resource[F[_], Source, Handle] {
  def using[A](s: Source, session: Handle => F[A]): F[A]
}

