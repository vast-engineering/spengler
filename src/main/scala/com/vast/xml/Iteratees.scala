package com.vast.xml

import com.typesafe.scalalogging.slf4j.Logging
import com.vast.util.iteratee._

/**
 * A set of low-level [[com.vast.util.iteratee.Iteratee]]s designed to help parse raw XML event streams.
 *
 * @author David Pratt (dpratt@vast.com)
 */
object Iteratees extends Logging {

  import Combinators._

  def ignoreElement: Iteratee[XMLEvent, Unit] = {

    def step: Iteratee[XMLEvent, Unit] = takeOne.flatMap {
      case x@StartElement(_, _) => ignoreElement.feed(Input.El(x)).flatMap(x => step)
      case EndElement(name) => Done(Unit, Input.Empty)
      case _ => step
    }
    expectStartElement.flatMap { startElem =>
      step
    }
  }

  /**
   * Return an Iteratee that will accept a StartElement event. The result
   * of the Iteratee will be the matched element.
   */
  def expectStartElement = expect[XMLEvent, StartElement] {
    case x@StartElement(_, _) => Right(x)
    case x => Left(s"Expected StartElement, got $x instead.")
  }

  def expectEndElement = expect[XMLEvent, EndElement] {
    case x@EndElement(_) => Right(x)
    case x => Left(s"Expected EndElement, got $x instead.")
  }

  def skipNonElement = {
    dropWhile[XMLEvent] {
      case x: StartElement => false
      case x: EndElement => false
      case _ => true
    }
  }

  def expectStartDoc = expect[XMLEvent, StartDocument.type] {
    case x@StartDocument => Right(x)
    case x => Left(s"Expected a StartDocument, got $x instead.")
  }

  def expectEndDoc = expect[XMLEvent, EndDocument.type] {
    case x@EndDocument =>
      Right(x)
    case x =>
      Left(s"Expected a EndDocument, got $x instead.")
  }

  def document[A](bodyContentHandler: Iteratee[XMLEvent, A]): Iteratee[XMLEvent, A] = {
    expectStartDoc.flatMap { startDoc =>
      bodyContentHandler.flatMap { body =>
        expectEndDoc.flatMap { endDoc =>
          Done[XMLEvent, A](body, Input.Empty)
        }
      }
    }
  }

  def textOnly: Iteratee[XMLEvent, Option[String]] = {

    expectStartElement.flatMap { startElementEvent =>
      def step(text: java.lang.StringBuilder): Iteratee[XMLEvent, Option[String]] = Cont {
        case Input.EOF =>
          Error(new IterateeException("Unexpected EOF."), Input.EOF)
        case Input.Empty => step(text)
        case in@Input.El(event) =>
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
