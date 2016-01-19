package ond
package service

import scala.collection.mutable
import scala.concurrent.{ ExecutionContext, Future }
import com.typesafe.scalalogging.StrictLogging

trait FutureService {
  def openSession(username: String, password: String)(implicit ec: ExecutionContext): Future[Long]
  def searchThings(sessionId: Long, searchTerm: String)(implicit ec: ExecutionContext): Future[List[Thing]]
  def getAThing(sessionId: Long, thingId: String)(implicit ec: ExecutionContext): Future[Thing]
  def saveAThing(sessionId: Long, thing: Thing)(implicit ec: ExecutionContext): Future[Unit]
  def closeSession(sessionId: Long)(implicit ec: ExecutionContext): Future[Unit]
}

class HappyFutureService(initialState: List[Thing]) extends FutureService with StrictLogging {
  val database = mutable.ListBuffer(initialState: _*)
  val sessions = mutable.ListBuffer.empty[Long]

  def openSession(username: String, password: String)(implicit ec: ExecutionContext) =
    Future {
      val newSession = System.nanoTime
      sessions += newSession
      logger.debug(s"Opened session $newSession")
      newSession
    }

  def searchThings(sessionId: Long, searchTerm: String)(implicit ec: ExecutionContext) =
    Future {
      logger.debug(s"Searching for items with id containing $searchTerm")
      database.filter { thing =>
        thing.id.toUpperCase.contains(searchTerm.toUpperCase)
      }.toList
    }

  def getAThing(sessionId: Long, thingId: String)(implicit ec: ExecutionContext) =
    Future {
      logger.debug(s"Retrieving item with id $thingId")
      database.filter { _.id == thingId }.head
    }

  def saveAThing(sessionId: Long, thing: Thing)(implicit ec: ExecutionContext) =
    Future {
      logger.debug(s"Saving item $thing")
      if (database.exists(_.id == thing.id)) ()
      else { database += thing; () }
    }

  def closeSession(sessionId: Long)(implicit ec: ExecutionContext) =
    Future {
      sessions -= sessionId
      logger.debug(s"Closing session $sessionId")
    }
}

