package ond
package service

import scala.collection.mutable
import com.typesafe.scalalogging.StrictLogging

trait CallbackService {
  def openSession(username: String, password: String, handler: Long => Unit): Unit
  def searchThings(sessionId: Long, searchTerm: String, handler: List[Thing] => Unit): Unit
  def getAThing(sessionId: Long, thingId: String, handler: Thing => Unit): Unit
  def saveAThing(sessionId: Long, thing: Thing, handler: () => Unit): Unit
  def closeSession(sessionId: Long): Unit
}

class HappyCallbackService(initialState: List[Thing]) extends CallbackService with StrictLogging {
  val database = mutable.ListBuffer(initialState: _*)
  val sessions = mutable.ListBuffer.empty[Long]

  def openSession(username: String, password: String, handler: Long => Unit): Unit = {
    val newSession = System.nanoTime
    sessions += newSession
    logger.debug(s"Opening session $newSession")
    handler(newSession)
  }

  def searchThings(sessionId: Long, searchTerm: String, handler: List[Thing] => Unit): Unit = {
    val relevantThings = database.filter { thing =>
      thing.id.toUpperCase.contains(searchTerm.toUpperCase)
    }
    logger.debug(s"Searching for items with id containing $searchTerm")
    handler(relevantThings.toList)
  }

  def getAThing(sessionId: Long, thingId: String, handler: Thing => Unit): Unit = {
    logger.debug(s"Retrieving item with id $thingId")
    handler(database.filter { _.id == thingId }.head)
  }

  def saveAThing(sessionId: Long, thing: Thing, handler: () => Unit): Unit = {
    logger.debug(s"Saving item $thing")
    if (database.exists(_.id == thing.id)) handler()
    else { database += thing; handler() }
  }

  def closeSession(sessionId: Long): Unit = {
    sessions -= sessionId
    logger.debug(s"Closing session $sessionId")
  }
}
