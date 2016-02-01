package ond

import ond.resource._
import ond.service._

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds

object `package` {
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  implicit def futureMonad(implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    def pure[A](a: A) = Future(a)
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa map f
    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  }

  implicit class MonadOps[F[_], A](fa: F[A])(implicit M: Monad[F]) {
    def map[B](f: A => B): F[B] = M.map(fa)(f)
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
  }

  implicit class ResourceOps[F[_]: Monad, A](a: A) {
    def run[AA >: A, B, C](s: Session[F, AA, B, C])(implicit R: Resource[F, AA, B]): F[C] = s.run(a)
    def run[AA >: A, B, C](s: SessionF[F, AA, B, C])(implicit R: Resource[F, AA, B]): F[C] = s.run(a)
  }

  type ThingSession[A] = Session[Future, ThingService, Long, A]
  def ThingSession[A](f: Long => A)(implicit ec: ExecutionContext) =
    Session[Future, ThingService, Long, A](f)

  type ThingSessionF[A] = SessionF[Future, ThingServiceF, Long, A]
  def ThingSessionF[A](f: Long => A)(implicit ec: ExecutionContext) =
    SessionF[Future, ThingServiceF, Long, A](f)

  implicit def thingServiceResource(implicit ec: ExecutionContext): Resource[Future, ThingService, Long] =
    new Resource[Future, ThingService, Long] {
      def using[A](s: ThingService, session: Long => Future[A]): Future[A] = {
        val getHandle = s.openSession("david", "fpnortheast")
        val runSession = getHandle flatMap session
        runSession onComplete { case _ => getHandle flatMap s.closeSession }
        runSession
      }
    }

  implicit def thingServiceFResource(implicit ec: ExecutionContext): Resource[Future, ThingServiceF, Long] =
    new Resource[Future, ThingServiceF, Long] {
      def using[A](s: ThingServiceF, session: Long => Future[A]): Future[A] = {
        val getHandle = s.openSession("david", "fpnortheast")
        val runSession = getHandle flatMap session
        runSession onComplete { case _ => getHandle flatMap s.closeSession }
        runSession
      }
    }
}
