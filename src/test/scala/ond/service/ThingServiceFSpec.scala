package ond
package service

import ond.resource._
import org.scalatest._
import org.scalatest.matchers._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Try, Success, Failure }

import scala.concurrent.ExecutionContext.Implicits.global

class ThingServiceFSpec extends FlatSpec with Matchers {
  val initialData = List(
    Thing("important"),
    Thing("essential"),
    Thing("indispensible"),
    Thing("irreplaceable")
  )

  "ThingService" should "search things" in {
    val thingService = new HappyThingServiceF(initialData)
    import thingService._

    thingService.run(searchThings("ble")) onComplete {
      case Success(things) =>
        things shouldBe List(Thing("indispensible"), Thing("irreplaceable"))
      case Failure(exc) =>
        fail(s"Couldn't search things due to $exc")
    }
  }

  it should "get a specific thing" in {
    val thingService = new HappyThingServiceF(initialData)
    import thingService._

    thingService.run(getAThing("essential")) onComplete {
      case Success(thing) => thing shouldBe Thing("essential")
      case Failure(exc) => fail(s"Couldn't get a thing due to $exc")
    }
  }

  it should "save a thing" in {
    val thingService = new HappyThingServiceF(initialData)
    val newThingId = "necessary"
    import thingService._

    val thingSave = for {
      _ <- saveAThing(Thing(newThingId))
      things <- searchThings(newThingId)
    } yield things

    thingService.run(thingSave) onComplete {
      case Success(things) => things shouldBe List(Thing("necessary"))
      case Failure(exc) => fail(s"Couldn't save a thing due to $exc")
    }
  }

  it should "unfortunately not be stack safe :(" in {
    val list = (0 to 100000).toList
    type Id[A] = A

    implicit val idMonad: Monad[Id] = new Monad[Id] {
      def pure[A](a: A) = a
      def map[A, B](a: A)(f: A => B): B = f(a)
      def flatMap[A, B](a: A)(f: A => B): B = f(a)
    }

    type TestSessionF[A] = SessionF[Id, Unit, Unit, A]
    def TestSessionF[A](f: Unit => A) =
      Session[Id, Unit, Unit, A](f)

    implicit def idResource: Resource[Id, Unit, Unit] =
      new Resource[Id, Unit, Unit] {
        type Source = Unit

        def using[A](s: Source, session: Unit => A): A =
          session(())
      }

    val notSafe = list.foldLeft(
      TestSessionF(h => List.empty[Thing])
    )((session, next) => for {
        listThing <- session
        thing <- TestSessionF(h => Thing(h.toString))
      } yield thing :: listThing)

    intercept[StackOverflowError] {
      notSafe.run(())
    }
  }

}
