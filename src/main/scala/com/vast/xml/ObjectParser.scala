package com.vast.xml

import com.vast.util.iteratee._
import Combinators._
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.control.NonFatal

object ObjectParser extends Logging {

  import Iteratees._

  def required[A](it: Iteratee[XMLEvent, Option[A]]) = {
    it.flatMap { value =>
      value.map(Done[XMLEvent, A](_)).getOrElse(Error(new IterateeException("Missing or blank required value."), Input.Empty))
    }
  }

  def xmlString = textOnly

  def xmlNumber = textOnly.flatMap { numberOpt =>
    try {
      Done(numberOpt.map(x => BigDecimal(x)))
    } catch {
      case e: NumberFormatException => Error(new IterateeException(s"Unable to parse $numberOpt into a number.", e), Input.Empty)
    }
  }

  def xmlObject[A, V](elementsAggregator: Iteratee[V, A],
                      elementHandlerProducer: (String, Map[String, String]) => Option[Iteratee[XMLEvent, V]]): Iteratee[XMLEvent, A] =
    for {
      _ <- expectStartElement
      _ <- skipNonElement
      elements <- takeOne[XMLEvent].flatMap {
        case x@EndElement(name) =>
          //if we've reached the end element, run the elementsHandler iteratee to extract the result value
          elementsAggregator.run.map(a => Done[XMLEvent, A](a)).recover({case NonFatal(e) => Error[XMLEvent](e, Input.Empty)}).get
        case x@StartElement(name, attrs) =>
          //not done yet - parse the element
          childElements(elementsAggregator, elementHandlerProducer).feed(Input.El(x))
        case x => Error[XMLEvent](new IterateeException(s"Expected StartElement or EndElement, but got $x instead."), Input.Empty)
      }
    } yield elements

  private[this] def childElements[A, V](elementsAggregator: Iteratee[V, A],
                                        elementHandlerProducer: (String, Map[String, String]) => Option[Iteratee[XMLEvent, V]]): Iteratee[XMLEvent, A] =
    for {
      _ <- skipNonElement
      fedAggregator <- {
        childElement(elementHandlerProducer).map { childValueOpt =>
          childValueOpt.map { childValue =>
            elementsAggregator.feed(Input.El(childValue))
          } getOrElse {
            elementsAggregator
          }
        }
      }
      _ <- skipNonElement
      parsedElements <- peekOne[XMLEvent].flatMap {
        case x: EndElement =>
          drop(1).flatMap { _ =>
            fedAggregator.run.map(a => Done[XMLEvent, A](a)).recover({ case NonFatal(e) => Error[XMLEvent](e, Input.Empty)}).get
          }
        case x: StartElement =>
          //call ourselves recursively to parse the next child element
          childElements(fedAggregator, elementHandlerProducer)
        case x => Error(new IterateeException(s"Unexpected event $x"), Input.Empty)
      }
    } yield parsedElements

  private[this] def childElement[V](elementHandlerProducer: (String, Map[String, String]) => Option[Iteratee[XMLEvent, V]]): Iteratee[XMLEvent, Option[V]] = {
    peekOne.flatMap {
      case StartElement(name, attrs) =>
        elementHandlerProducer(name, attrs).map { handler =>
          handler.map(x => Some(x))
        } getOrElse {
          //if there's no handler, consume the element and return None
          ignoreElement.map(x => None)
        }
      case x => Error(new IterateeException(s"Expected StartElement, got $x instead"), Input.Empty)
    }
  }

  def parseObject[V](childHandlers: (String, Iteratee[XMLEvent, V])*): Enumeratee[XMLEvent, V] =
    parseObject(Map(childHandlers: _*))

  def parseObject[V](childHandlers: Map[String, Iteratee[XMLEvent, V]]): Enumeratee[XMLEvent, V] =
    parseObject { (name, attrs) =>
      //logger.debug("Getting entry for {}", name)
      childHandlers.get(name)
    }

  type PF[V] = PartialFunction[(String, Map[String, String]), Iteratee[XMLEvent, V]]
  def parser[V](pf: PF[V]) = Function.untupled(pf.lift)
  def parseObject[V](pf: PF[V]): Enumeratee[XMLEvent, V] = parseObject(parser(pf))



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
  def parseObject[V](childHandlerProducer: (String, Map[String, String]) => Option[Iteratee[XMLEvent, V]]): Enumeratee[XMLEvent, V] = new Enumeratee[XMLEvent, V] {

    def step[A](inner: Iteratee[V, A])(in: Input[V]): Iteratee[V, Iteratee[V, A]] = in match {
      case Input.EOF => Done(inner, in)
      case _ => Cont(step(inner.feed(in)))
    }

    def applyOn[A](inner: Iteratee[V, A]): Iteratee[XMLEvent, Iteratee[V, A]] = {
      xmlObject(Cont(step(inner)), childHandlerProducer)
    }
  }

}
