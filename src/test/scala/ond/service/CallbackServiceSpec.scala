package ond
package service

import ond.resource._
import org.scalatest._
import org.scalatest.matchers._

class CallbackServiceSpec extends FlatSpec with Matchers {
  val initialData = List(
    Thing("important"),
    Thing("essential"),
    Thing("indispensible"),
    Thing("irreplaceable")
  )

  "CallbackService" should "search things" in {
    val callbackService = new HappyCallbackService(initialData)
    import callbackService._

    openSession("david", "fpnortheast", sid => {
      searchThings(sid, "ble", things => {
        closeSession(sid)
        things shouldBe List(Thing("indispensible"), Thing("irreplaceable"))
      })
    })
  }

  it should "get a specific thing" in {
    val callbackService = new HappyCallbackService(initialData)
    import callbackService._

    openSession("david", "fpnortheast", sid => {
      getAThing(sid, "essential", thing => {
        closeSession(sid)
        thing shouldBe Thing("essential")
      })
    })
  }

  it should "save a thing" in {
    val callbackService = new HappyCallbackService(initialData)
    val newThingId = "necessary"
    import callbackService._

    openSession("david", "fpnortheast", sid => {
      saveAThing(sid, Thing(newThingId), () => {
        searchThings(sid, newThingId, things => {
          closeSession(sid)
          things shouldBe List(Thing("necessary"))
        })
      })
    })
  }

}
