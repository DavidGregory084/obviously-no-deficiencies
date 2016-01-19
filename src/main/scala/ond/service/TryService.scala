package ond
package service

import scala.collection.mutable
import scala.util.{ Try, Success, Failure }
import com.typesafe.scalalogging.StrictLogging

trait TryService {
  def openSession(username: String, password: String): Try[Long]
  def searchThings(sessionId: Long, searchTerm: String): Try[List[Thing]]
  def getAThing(sessionId: Long, thingId: String): Try[Thing]
  def saveAThing(sessionId: Long, thing: Thing): Try[Unit]
  def closeSession(sessionId: Long): Try[Unit]
}

class HappyTryService(initialState: List[Thing]) extends TryService with StrictLogging {
  val database = mutable.ListBuffer(initialState: _*)
  val sessions = mutable.ListBuffer.empty[Long]

  def openSession(username: String, password: String) = Try {
    val newSession = System.nanoTime
    sessions += newSession
    logger.debug(s"Opening session $newSession")
    newSession
  }

  def searchThings(sessionId: Long, searchTerm: String) = Try {
    logger.debug(s"Searching for items with id containing $searchTerm")
    database.filter { thing =>
      thing.id.toUpperCase.contains(searchTerm.toUpperCase)
    }.toList
  }

  def getAThing(sessionId: Long, thingId: String) = Try {
    logger.debug(s"Retrieving item with id $thingId")
    database.filter { _.id == thingId }.head
  }

  def saveAThing(sessionId: Long, thing: Thing) = Try {
    logger.debug(s"Saving item $thing")
    if (database.exists(_.id == thing.id)) ()
    else { database += thing; () }
  }

  def closeSession(sessionId: Long) = Try {
    sessions -= sessionId
    logger.debug(s"Closing session $sessionId")
  }
}

