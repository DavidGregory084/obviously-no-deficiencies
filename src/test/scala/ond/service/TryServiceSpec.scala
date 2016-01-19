package ond
package service

import ond.resource._
import org.scalatest._
import org.scalatest.matchers._
import scala.util.{ Try, Success, Failure }

class TryServiceSpec extends FlatSpec with Matchers {
  val initialData = List(
    Thing("important"),
    Thing("essential"),
    Thing("indispensible"),
    Thing("irreplaceable")
  )

  "TryService" should "search things" in {
    val tryService = new HappyTryService(initialData)
    import tryService._

    /* Identical to:
     * {{{
     * val trySearch =
     *   openSession("david", "fpnortheast") flatMap { sid =>
     *     searchThings(sid, "ble") flatMap { things =>
     *       closeSession(sid) map (_ => things)
     *     }
     *   }
     * }}}
     */
    val trySearch = for {
      sid <- openSession("david", "fpnortheast")
      things <- searchThings(sid, "ble")
      _ <- closeSession(sid)
    } yield things

    trySearch match {
      case Success(things) =>
        things shouldBe List(Thing("indispensible"), Thing("irreplaceable"))
      case Failure(exc) =>
        fail(s"Couldn't search things due to $exc")
    }
  }

  it should "get a specific thing" in {
    val tryService = new HappyTryService(initialData)
    import tryService._

    val tryGet = for {
      sid <- openSession("david", "fpnortheast")
      thing <- getAThing(sid, "essential")
      _ <- closeSession(sid)
    } yield thing

    tryGet match {
      case Success(thing) => thing shouldBe Thing("essential")
      case Failure(exc) => fail(s"Couldn't get a thing due to $exc")
    }
  }

  it should "save a thing" in {
    val tryService = new HappyTryService(initialData)
    val newThingId = "necessary"
    import tryService._

    val trySave = for {
      sid <- openSession("david", "fpnortheast")
      _ <- saveAThing(sid, Thing(newThingId))
      things <- searchThings(sid, newThingId)
      _ <- closeSession(sid)
    } yield things

    trySave match {
      case Success(things) => things shouldBe List(Thing("necessary"))
      case Failure(exc) => fail(s"Couldn't save a thing due to $exc")
    }
  }

}
