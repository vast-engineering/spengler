package com.vast.xml

import com.typesafe.scalalogging.slf4j.Logging
import javax.xml.stream.XMLStreamConstants._
import javax.xml.stream.XMLStreamReader
import scala.collection.mutable

sealed trait XMLEvent
object XMLEvent extends Logging {

  def apply(reader: XMLStreamReader): XMLEvent = {
    reader.getEventType match {
      case START_DOCUMENT =>
        StartDocument
      case START_ELEMENT =>
        val attrCount = reader.getAttributeCount
        var idx = 0
        val attrs = new mutable.HashMap[String, String]()
        while(idx < attrCount) {
          attrs.put(reader.getAttributeLocalName(idx), reader.getAttributeValue(idx))
          idx = idx + 1
        }
        StartElement(reader.getLocalName, attrs.toMap)
      case END_ELEMENT =>
        EndElement(reader.getLocalName)
      case CHARACTERS =>
        Characters(reader.getText)
      case CDATA =>
        CData(reader.getText)
      case END_DOCUMENT =>
        EndDocument
      case _ =>
        UnknownEvent(reader.getEventType)
    }
  }
}

case object StartDocument extends XMLEvent
case object EndDocument extends XMLEvent
case class StartElement(name: String, attributes: Map[String, String]) extends XMLEvent
case class EndElement(name: String) extends XMLEvent
case class Characters(data: String) extends XMLEvent
case class CData(data: String) extends XMLEvent
case class UnknownEvent(eventType: Int) extends XMLEvent

