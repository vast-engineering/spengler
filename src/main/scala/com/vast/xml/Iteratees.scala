package com.vast.xml

import com.typesafe.scalalogging.slf4j.Logging
import play.api.libs.iteratee._
import scala.concurrent.{ExecutionContext, Future}
import com.vast.xml.ObjectParser._
import scala.Some

/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
object Iteratees extends Logging {

  import Combinators._

  def consumeElement(implicit ec: ExecutionContext): Iteratee[XMLEvent, Unit] = xmlObject(Iteratee.ignore, (name, attrs) => Some(consumeElement))

  /**
   * Return an Iteratee that will accept a StartElement event. The result
   * of the Iteratee will be the matched element.
   */
  def expectStartElement(implicit ec: ExecutionContext): Iteratee[XMLEvent, StartElement] = {
    expect {
      case x@StartElement(_, _) => Right(x)
      case x => Left(s"Expected StartElement, got $x instead.")
    }
  }

  def expectEndElement(implicit ec: ExecutionContext) = {
    expect {
      case x@EndElement(_) => Right(x)
      case x => Left(s"Expected EndElement, got $x instead.")
    }
  }

  def skipNonElement = {
    dropWhile {
      case x: StartElement => false
      case x: EndElement => false
      case _ => true
    }
  }

  def document[A](bodyContentHandler: Iteratee[XMLEvent, A])(implicit ec: ExecutionContext): Iteratee[XMLEvent, A] = {
    val startDocScanner = expect {
      case StartDocument => Right(true)
      case x => Left(s"Expected a StartDocument, got $x instead.")
    }

    val body = Enumeratee.takeWhile[XMLEvent](_ != EndDocument).transform(bodyContentHandler)

    for {
      _ <- startDocScanner
      parsed <- body
    } yield parsed
  }

  def textOnly(implicit ec: ExecutionContext): Iteratee[XMLEvent, Option[String]] = {

    expectStartElement.flatMap { startElementEvent =>
      def step(text: StringBuilder): Iteratee[XMLEvent, Option[String]] = Cont {
        case Input.EOF => Error("Unexpected EOF.", Input.EOF)
        case Input.Empty => step(text)
        case in@Input.El(event) =>
          event match {
            //parse text
            case Characters(data) => step(text.append(data))
            case CData(data) => step(text.append(data))
            case EndElement(name) =>
              if (name != startElementEvent.name) {
                //we got an unexpected end element
                Error(s"Unexpected EndElement($name) - only EndElement(${startElementEvent.name} is valid in this position.", Input.Empty)
              } else {
                val rawText = text.toString().trim
                Done(if (rawText.isEmpty) None else Some(rawText))
              }
            case UnknownEvent(eventType) =>
              logger.debug(s"Skipping element of type $eventType")
              step(text) //do nothing - skip the element
            case startElem@StartElement(name, attrs) =>
              Error(s"Text only nodes cannot have content.", Input.Empty)
            case x => Error(s"Unexpected event $x.", in)
          }
      }

      step(new StringBuilder)
    }

  }

}
