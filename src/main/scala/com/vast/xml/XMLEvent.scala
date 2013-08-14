package com.vast.xml

import com.typesafe.scalalogging.slf4j.Logging
import javax.xml.stream.XMLStreamConstants._
import scala.collection.immutable
import javax.xml.stream.XMLStreamReader

//Classes that wrap XML events from a stream reader. Using objects like this imposes a small performance
//hit on top of reading the raw stream inline, in that every event needs to be fully parsed. Normally, an
//XMLStreamReader avoids object allocation if fields are not asked for - for example, this implementation
//fully reads and parses attributes from every START_ELEMENT event, even if they are ultimately ignored. A
//raw stream reader woud just skip over the bytes in the stream.
sealed trait XMLEvent

object XMLEvent extends Logging {

  def apply(reader: XMLStreamReader): XMLEvent = {
    reader.getEventType match {
      case START_DOCUMENT =>
        StartDocument
      case START_ELEMENT =>
        startElement(reader)
      case END_ELEMENT =>
        EndElement(reader.getLocalName)
      case CHARACTERS =>
        Characters(getCharBuffer(reader))
      case CDATA =>
        CData(getCharBuffer(reader))
      case END_DOCUMENT =>
        EndDocument
      case _ =>
        UnknownEvent(reader.getEventType)
    }
  }

  //we don't convert to Strings yet because we may or may not ultimately need the actual String
  //created - just leave it as a char buffer for now
  def getCharBuffer(reader: XMLStreamReader) = {
    val len = reader.getTextLength
    val buffer = new Array[Char](len)
    reader.getTextCharacters(reader.getTextStart, buffer, 0, len)
    buffer
  }

  def startElement(reader: XMLStreamReader) = {
    val attrCount = reader.getAttributeCount
    var idx = 0
//    java hashmaps are still faster than Scala's - use it here
    val attrs = new java.util.HashMap[String, String]()
    while (idx < attrCount) {
      attrs.put(reader.getAttributeLocalName(idx), reader.getAttributeValue(idx))
      idx = idx + 1
    }
    StartElement(reader.getLocalName, toImmutable(attrs))
  }

  //this code is ugly (uses iteration) but it needs to be fast
  //this converts a java Map to an immutable Scala map.
  private[this] def toImmutable[T, U](m: java.util.Map[T, U]): immutable.Map[T, U] = {
    val b = immutable.Map.newBuilder[T, U]
    val it = m.entrySet().iterator()
    while(it.hasNext) {
      val entry = it.next()
      val pair = (entry.getKey, entry.getValue)
      b += pair
    }
    b.result()
  }

}

case object StartDocument extends XMLEvent

case object EndDocument extends XMLEvent

case class StartElement(name: String, attributes: Map[String, String]) extends XMLEvent

case class EndElement(name: String) extends XMLEvent

case class Characters(data: Array[Char]) extends XMLEvent

case class CData(data: Array[Char]) extends XMLEvent

case class UnknownEvent(eventType: Int) extends XMLEvent

