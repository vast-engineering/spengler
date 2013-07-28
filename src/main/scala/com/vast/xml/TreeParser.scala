package com.vast.xml

import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext
import com.typesafe.scalalogging.slf4j.Logging

object TreeParser extends Logging {

  import Iteratees._
  sealed trait XMLNode
  case class XMLDocument(root: XMLElement) extends XMLNode
  case class XMLElement(name: String, attributes: Map[String, String], children: Seq[XMLElement], text: Option[String]) extends XMLNode

  def xmlDocument(implicit ec: ExecutionContext) = document(xmlElement).map(root => XMLDocument(root))

  def xmlElement(implicit ec: ExecutionContext): Iteratee[XMLEvent, XMLElement] = {

    expectStartElement.flatMap { startElementEvent =>
      def step(text: StringBuilder, children: IndexedSeq[XMLElement]): Iteratee[XMLEvent, XMLElement] = Cont {
        case in@Input.EOF => Error("Unexpected EOF.", in)
        case in@Input.Empty => step(text, children)
        case in@Input.El(event) => event match {
          //parse text
          case Characters(data) => step(text.append(data), children)
          case CData(data) => step(text.append(data), children)
          case startElem@StartElement(name, attrs) => Iteratee.flatten(xmlElement.feed(Input.El(startElem))).flatMap { child =>
            step(text, children :+ child)
          }
          case EndElement(name) =>
            if (name != startElementEvent.name) {
              //we got an unexpected end element
              Error(s"Unexpected EndElement($name) - only EndElement(${startElementEvent.name} is valid in this position.", Input.Empty)
            } else {
              val rawText = text.toString().trim
              Done(XMLElement(startElementEvent.name, startElementEvent.attributes, children, if(rawText.isEmpty) None else Some(rawText)))
            }
          case UnknownEvent(eventType) =>
            logger.debug(s"Skipping element of type $eventType")
            step(text, children) //do nothing - skip the element
          case x =>
            Error("Invalid event $x encountered.", Input.Empty)
        }
      }

      step(new StringBuilder, IndexedSeq())
    }

  }
}
