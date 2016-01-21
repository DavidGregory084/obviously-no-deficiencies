package ond
package service

import scala.collection.mutable
import scala.concurrent.{ ExecutionContext, Future }
import com.typesafe.scalalogging.StrictLogging

trait ThingServiceF {
  def openSession(username: String, password: String)(implicit ec: ExecutionContext): Future[Long]
  def searchThings(searchTerm: String)(implicit ec: ExecutionContext): ThingSessionF[List[Thing]]
  def getAThing(thingId: String)(implicit ec: ExecutionContext): ThingSessionF[Thing]
  def saveAThing(thing: Thing)(implicit ec: ExecutionContext): ThingSessionF[Unit]
  def closeSession(sessionId: Long)(implicit ec: ExecutionContext): Future[Unit]
}

class HappyThingServiceF(initialState: List[Thing]) extends ThingServiceF with StrictLogging {
  val database = mutable.ListBuffer(initialState: _*)
  val sessions = mutable.ListBuffer.empty[Long]

  def openSession(username: String, password: String)(implicit ec: ExecutionContext) =
    Future {
      val newSession = System.nanoTime
      sessions += newSession
      logger.debug(s"Opening session $newSession")
      newSession
    }

  def searchThings(searchTerm: String)(implicit ec: ExecutionContext) = ThingSessionF { sid =>
    Future {
      logger.debug(s"Searching for items with id containing $searchTerm")
      database.filter { thing =>
        thing.id.toUpperCase.contains(searchTerm.toUpperCase)
      }.toList
    }
  }

  def getAThing(thingId: String)(implicit ec: ExecutionContext) = ThingSessionF { sid =>
    Future {
      logger.debug(s"Retrieving item with id $thingId")
      database.filter { _.id == thingId }.head
    }
  }

  def saveAThing(thing: Thing)(implicit ec: ExecutionContext) = ThingSessionF { sid =>
    Future {
      logger.debug(s"Saving item $thing")
      if (database.exists(_.id == thing.id)) ()
      else { database += thing; () }
    }
  }

  def closeSession(sessionId: Long)(implicit ec: ExecutionContext) =
    Future {
      sessions -= sessionId
      logger.debug(s"Closing session $sessionId")
    }
}

