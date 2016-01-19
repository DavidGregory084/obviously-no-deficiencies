package ond
package service

import ond.resource._
import org.scalatest._
import org.scalatest.matchers._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Try, Success, Failure }

import scala.concurrent.ExecutionContext.Implicits.global

class ThingServiceSpec extends FlatSpec with Matchers {
  val initialData = List(
    Thing("important"),
    Thing("essential"),
    Thing("indispensible"),
    Thing("irreplaceable")
  )

  "ThingService" should "search things" in {
    val thingService = new HappyThingService(initialData)
    import thingService._

    thingService.run(searchThings("ble")) onComplete {
      case Success(things) =>
        things shouldBe List(Thing("indispensible"), Thing("irreplaceable"))
      case Failure(exc) =>
        fail(s"Couldn't search things due to $exc")
    }
  }

  it should "get a specific thing" in {
    val thingService = new HappyThingService(initialData)
    import thingService._

    thingService.run(getAThing("essential")) onComplete {
      case Success(thing) => thing shouldBe Thing("essential")
      case Failure(exc) => fail(s"Couldn't get a thing due to $exc")
    }
  }

  it should "save a thing" in {
    val thingService = new HappyThingService(initialData)
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

}
