package ond
package service

import ond.resource._
import org.scalatest._
import org.scalatest.matchers._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Try, Success, Failure }

import scala.concurrent.ExecutionContext.Implicits.global

class FutureServiceSpec extends FlatSpec with Matchers {
  val initialData = List(
    Thing("important"),
    Thing("essential"),
    Thing("indispensible"),
    Thing("irreplaceable")
  )

  "FutureService" should "search things" in {
    val futureService = new HappyFutureService(initialData)
    import futureService._

    /* Identical to:
     * {{{
     * val futureSearch =
     *   openSession("david", "fpnortheast") flatMap { sid =>
     *     searchThings(sid, "ble") flatMap { things =>
     *       closeSession(sid) map (_ => things)
     *     }
     *   }
     * }}}
     */
    val futureSearch = for {
      sid <- openSession("david", "fpnortheast")
      things <- searchThings(sid, "ble")
      _ <- closeSession(sid)
    } yield things

    futureSearch onComplete {
      case Success(things) =>
        things shouldBe List(Thing("indispensible"), Thing("irreplaceable"))
      case Failure(exc) =>
        fail(s"Couldn't search things due to $exc")
    }
  }

  it should "get a specific thing" in {
    val futureService = new HappyFutureService(initialData)
    import futureService._

    val futureGet = for {
      sid <- openSession("david", "fpnortheast")
      thing <- getAThing(sid, "essential")
      _ <- closeSession(sid)
    } yield thing

    futureGet onComplete {
      case Success(thing) => thing shouldBe Thing("essential")
      case Failure(exc) => fail(s"Couldn't get a thing due to $exc")
    }
  }

  it should "save a thing" in {
    val futureService = new HappyFutureService(initialData)
    val newThingId = "necessary"
    import futureService._

    val futureSave = for {
      sid <- openSession("david", "fpnortheast")
      _ <- saveAThing(sid, Thing(newThingId))
      things <- searchThings(sid, newThingId)
      _ <- closeSession(sid)
    } yield things

    futureSave onComplete {
      case Success(things) => things shouldBe List(Thing("necessary"))
      case Failure(exc) => fail(s"Couldn't save a thing due to $exc")
    }
  }

  it should "have realistic error handling" in {
    val futureService = new HappyFutureService(initialData)
    val newThingId = "necessary"
    import futureService._

    val futureOpenSession = openSession("david", "fpnortheast")

    val futureSave = for {
      sid <- futureOpenSession
      _ <- saveAThing(sid, Thing(newThingId))
      things <- searchThings(sid, newThingId)
    } yield things

    futureSave onComplete {
      case _ => futureOpenSession flatMap { sid => closeSession(sid) }
    }

    futureSave onComplete {
      case Success(things) =>
        things shouldBe List(Thing("necessary"))
      case Failure(exc) => fail(s"Couldn't save a thing due to $exc")
    }
  }

}
