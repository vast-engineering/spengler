package com.vast.xml

import play.api.libs.iteratee._
import play.api.libs.iteratee.Input.EOF
import com.vast.xml.Combinators._
import scala.concurrent.{ExecutionContext, Future}
import scala.Predef._

object ObjectParser {

  import Iteratees._

  def required[A](it: Iteratee[XMLEvent, Option[A]])(implicit ec: ExecutionContext) = {
    it.flatMap { value =>
      value.map(Done[XMLEvent, A](_)).getOrElse(Error("Missing or blank required value.", Input.Empty))
    }
  }

  def xmlString(implicit ec: ExecutionContext) = textOnly

  def xmlNumber(implicit ec: ExecutionContext) = textOnly.flatMap { numberOpt =>
    try {
      Done(numberOpt.map(x => BigDecimal(x)))
    } catch {
      case e: NumberFormatException => Error(s"Unable to parse $numberOpt into a number.", Input.Empty)
    }
  }

  def xmlObject[A, V](elementsAggregator: Iteratee[V, A],
                      elementHandlerProducer: (String, Map[String, String]) => Option[Iteratee[XMLEvent, V]])(implicit ec: ExecutionContext): Iteratee[XMLEvent, A] =
    for {
      _ <- expectStartElement
      _ <- skipNonElement
      elements <- peekOne.flatMap {
        case EndElement(_) =>
          //if we've reached the end element, run the elementsHandler iteratee to extract the result value
          drop(1).flatMap(_ => Iteratee.flatten(extractValue(elementsAggregator).map(a => Done[XMLEvent, A](a))))
        case x: StartElement =>
          //not done yet - parse the element
          childElements(elementsAggregator, elementHandlerProducer)
        case x => Error(s"Expected StartElement or EndElement, but got $x instead.", Input.Empty)
      }
    } yield elements

  private[this] def childElements[A, V](elementsAggregator: Iteratee[V, A],
                                        elementHandlerProducer: (String, Map[String, String]) => Option[Iteratee[XMLEvent, V]])(implicit ec: ExecutionContext): Iteratee[XMLEvent, A] =
    for {
      _ <- skipNonElement
      fedAggregator <- {
        peekOne.flatMap {
          case StartElement(name, attrs) =>
            elementHandlerProducer(name, attrs).map { handler =>
              handler.map(parsed => Iteratee.flatten(elementsAggregator.feed(Input.El(parsed))))
            } getOrElse {
              //if there's no handler, consume the element and just return the existing aggregator
              consumeElement.map(consumed => elementsAggregator)
            }
          case x => Error(s"Expected StartElement, got $x instead", Input.Empty)
        }
      }
      _ <- skipNonElement
      parsedElements <- peekOne.flatMap {
        case x: EndElement => Iteratee.flatten(extractValue(fedAggregator).map(a => Done[XMLEvent, A](a)))
        case x: StartElement =>
          //call ourselves recursively to parse the next child element
          childElements(fedAggregator, elementHandlerProducer)
        case x => Error(s"Unexpected event $x", Input.Empty)
      }
    } yield parsedElements


  def parseObject[V](childHandlers: (String, Iteratee[XMLEvent, V])*)(implicit ec: ExecutionContext): Enumeratee[XMLEvent, V] =
    parseObject(Map(childHandlers: _*))

  def parseObject[V](childHandlers: Map[String, Iteratee[XMLEvent, V]])(implicit ec: ExecutionContext): Enumeratee[XMLEvent, V] =
    parseObject { (name, attrs) =>
      childHandlers.get(name)
    }

  /**
   * Transforms a stream of XMLEvent to a stream of A.
   *
   * This can be used to parse an arbitrary tree of XML. It's assumed that the first XMLEvent received
   * is the StartElement for the parent tag. The childHandler function takes in info about a child element
   * and returns an Iteratee that can parse it.
   *
   * This Enumeratee is *very* useful when the input document is very large and the goal is to parse each
   * element in place and then discard the input.
   *
   */
  def parseObject[V](childHandlerProducer: (String, Map[String, String]) => Option[Iteratee[XMLEvent, V]])(implicit ec: ExecutionContext): Enumeratee[XMLEvent, V] = new Enumeratee[XMLEvent, V] {

    def step[A](inner: Iteratee[V, A])(in: Input[V]): Iteratee[V, Iteratee[V, A]] = in match {
      case EOF => Done(inner, in)
      case _ => Cont(step(Iteratee.flatten(inner.feed(in))))
    }

    def applyOn[A](inner: Iteratee[V, A]): Iteratee[XMLEvent, Iteratee[V, A]] = {
      xmlObject(Cont(step(inner)), childHandlerProducer)
    }
  }


  //A variant on Iteratee.run that does not directly throw exceptions, but rather complete the future with failures
  private def extractValue[A, B](it: Iteratee[A, B])(implicit ec: ExecutionContext): Future[B] = {
    it.fold {
      case Step.Cont(k) =>
        //if it's not done, send an EOF and hope that it finishes after that
        k(Input.EOF).fold {
          case Step.Cont(_) => Future.failed(new RuntimeException("Expected valuesHandler Iteratee to complete after EOF."))
          case Step.Done(x, _) => Future.successful(x)
          case Step.Error(msg, _) => Future.failed(new RuntimeException(msg))
        }
      case Step.Done(x, _) => Future.successful(x)
      case Step.Error(msg, _) => Future.failed(new RuntimeException(msg))
    }
  }


}
