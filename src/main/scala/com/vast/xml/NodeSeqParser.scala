package com.vast.xml

import scala.xml._

import com.typesafe.scalalogging.slf4j.Logging
import com.vast.util.iteratee._

/**
 * An [[com.vast.util.iteratee.Iteratee]] that parses xml into a Scala NodeSeq.
 *
 * Note - for now, this parser does *not* support XML namespaces.
 */
object NodeSeqParser extends Logging {

  import com.vast.xml.Iteratees._

  def parseDocument: Iteratee[XMLEvent, NodeSeq] = for {
    startDoc <- expectStartDoc
    nodeSeq <- parseNodeSeq
    endDoc <- expectEndDoc
  } yield {
    nodeSeq
  }

  def parseNodeSeq: Iteratee[XMLEvent, NodeSeq] = parseNode

  private[this] def parseNode: Iteratee[XMLEvent, Node] = {
    expectStartElement.flatMap { startElementEvent =>
      def step(text: java.lang.StringBuilder, children: List[Node]): Iteratee[XMLEvent, Node] = Cont {
        case in@Input.EOF =>
          Error(new IterateeException("Unexpected EOF."), in)
        case in@Input.Empty => step(text, children)
        case in@Input.El(event) => event match {
          //parse text
          case Characters(data) => step(text.append(data), children)
          case CData(data) => step(text.append(data), children)
          case startElem@StartElement(name, attrs) => parseNode.feed(Input.El(startElem)).flatMap { child =>
            step(text, children :+ child)
          }
          case EndElement(name) =>
            if (name != startElementEvent.name) {
              //we got an unexpected end element
              Error(new IterateeException(s"Unexpected EndElement($name) - only EndElement(${startElementEvent.name} is valid in this position."), Input.Empty)
            } else {
              //add the contained text to the children as a Text node
              val textChildren = Text(text.toString.trim) :: children
              //explicitly do not support XML namespaces
              val node = Elem(null, startElementEvent.name, convertAttrs(startElementEvent), TopScope, minimizeEmpty = false, textChildren: _*)
              Done(node)
            }
          case UnknownEvent(eventType) =>
            logger.debug(s"Skipping element of type $eventType")
            step(text, children) //do nothing - skip the element
          case x =>
            Error(new IterateeException("Invalid event $x encountered."), Input.Empty)
        }
      }

      step(new java.lang.StringBuilder(100), List())
    }

  }

  private[this] def convertAttrs(start: StartElement): MetaData = {
    start.attributes.foldLeft[MetaData](Null) {
      case (previousAttr, (key, value)) => Attribute(null, key, value, previousAttr)
    }
  }


}
