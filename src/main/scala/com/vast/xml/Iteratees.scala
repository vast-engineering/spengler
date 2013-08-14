package com.vast.xml

import com.typesafe.scalalogging.slf4j.Logging
import com.vast.util.iteratee._
import scala.annotation.tailrec

/**
 * A set of low-level [[com.vast.util.iteratee.Iteratee]]s designed to help parse raw XML event streams.
 *
 * @author David Pratt (dpratt@vast.com)
 */
object Iteratees extends Logging {

  import Combinators._

  def ignoreElement: Iteratee[XMLEvent, Unit] = {

    def step(depth: Int): Iteratee[XMLEvent, Unit] = Cont {
      case Input.El(event) => event match {
        case x: StartElement => step(depth + 1)
        case x: EndElement =>
          val newDepth = depth - 1
          if(newDepth == 0) {
            Done(Unit, Input.Empty)
          } else {
            step(newDepth)
          }
        case _ => step(depth)
      }
      case Input.EOF =>
        Error(new IterateeException("Unexpected EOF in ignoreElement"), Input.Empty)
      case Input.Empty => step(depth)
    }

    for {
      startElem <- expectStartElement
      res <- step(1)
    } yield res
  }

  /**
   * Return an Iteratee that will accept a StartElement event. The result
   * of the Iteratee will be the matched element.
   */
  def expectStartElement = expect[XMLEvent](_.isInstanceOf[StartElement]).map(_.asInstanceOf[StartElement])

  def expectEndElement = expect[XMLEvent](_.isInstanceOf[EndElement]).map(_.asInstanceOf[EndElement])

  def skipToNextStartOrEnd: Iteratee[XMLEvent, XMLEvent] = Cont {
    case Input.El(event) => if( event.isInstanceOf[StartElement] || event.isInstanceOf[EndElement] ) Done(event) else skipToNextStartOrEnd
    case Input.EOF => Error(new IterateeException("Expected StartElement or EndElement, got EOF instead."), Input.Empty)
    case _ => skipToNextStartOrEnd
  }

  def expectStartDoc = expect[XMLEvent](_ == StartDocument)

  def expectEndDoc = expect[XMLEvent](_ == EndDocument)

  def document[A](bodyContentHandler: Iteratee[XMLEvent, A]): Iteratee[XMLEvent, A] = for {
    _ <- expectStartDoc
    body <- bodyContentHandler
    _ <- expectEndDoc
  } yield {
    body
  }

  def textOnly: Iteratee[XMLEvent, Option[String]] = {

    expectStartElement.flatMap { startElementEvent =>
      def step(text: java.lang.StringBuilder): Iteratee[XMLEvent, Option[String]] = Cont {
        case Input.EOF =>
          Error(new IterateeException("Unexpected EOF."), Input.EOF)
        case Input.Empty => step(text)
        case Input.El(event) =>
          event match {
            //parse text
            case Characters(data) =>
              step(text.append(data))
            case CData(data) =>
              step(text.append(data))
            case EndElement(name) =>
              if (name != startElementEvent.name) {
                //we got an unexpected end element
                Error(new IterateeException(s"Unexpected EndElement($name) - only EndElement(${startElementEvent.name} is valid in this position."), Input.Empty)
              } else {
                val rawText = text.toString.trim
                Done(if (rawText.isEmpty) None else Some(rawText))
              }
            case UnknownEvent(eventType) =>
              logger.debug(s"Skipping element of type $eventType")
              step(text) //do nothing - skip the element
            case startElem@StartElement(name, attrs) =>
              Error(new IterateeException(s"Text only nodes cannot have content."), Input.Empty)
            case x => Error(new IterateeException(s"Unexpected event $x."), Input.Empty)
          }
      }

      step(new java.lang.StringBuilder(100))
    }

  }

}
