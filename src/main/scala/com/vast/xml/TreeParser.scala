package com.vast.xml

import com.vast.util.iteratee._
import com.typesafe.scalalogging.LazyLogging

/**
 * A set of [[com.vast.util.iteratee.Iteratee]]s that can parse a stream of [[com.vast.xml.XMLEvent]]s into
 * a tree of Node instances. This is intended to be a very lightweight representation of the root element in-memory - if you
 * need more complex behavior, please use the NodeSeqParser.
 *
 */
object TreeParser extends LazyLogging {

  import Iteratees._

  sealed trait XMLNode
  case class XMLDocument(root: XMLElement) extends XMLNode

  case class XMLElement(name: String, attributes: Map[String, String], children: Seq[XMLElement], text: Option[String]) extends XMLNode

  /**
   * Parse an entire XML document.
   */
  def xmlDocument = document(xmlElement).map { root =>
    XMLDocument(root)
  }

  /**
   * An Iteratee that parses the tree of XMLElements rooted at the current element. This Iteratee assumes that it's first
   * input will be a [[com.vast.xml.StartElement]] event.
   *
   */
  def xmlElement = parseTree

  private[this] def parseTree: Iteratee[XMLEvent, XMLElement] = {
    expectStartElement.flatMap { startElementEvent =>
      def step(text: java.lang.StringBuilder, children: IndexedSeq[XMLElement]): Iteratee[XMLEvent, XMLElement] = Cont {
        case Input.EOF =>
          Error(new IterateeException("Unexpected EOF."), Input.EOF)
        case Input.Empty => step(text, children)
        case Input.El(event) => event match {
          //parse text
          case Characters(data) => step(text.append(data), children)
          case CData(data) => step(text.append(data), children)
          case startElem: StartElement => parseTree.feed(Input.El(startElem)).flatMap { child =>
            step(text, children :+ child)
          }
          case EndElement(name) =>
            if (name != startElementEvent.name) {
              //we got an unexpected end element
              Error(new IterateeException(s"Unexpected EndElement($name) - only EndElement(${startElementEvent.name} is valid in this position."), Input.Empty)
            } else {
              val rawText = text.toString.trim
              Done(XMLElement(startElementEvent.name, startElementEvent.attributes, children, if(rawText.isEmpty) None else Some(rawText)))
            }
          case x: UnknownEvent =>
            logger.debug(s"Skipping element of type $x")
            step(text, children) //do nothing - skip the element
          case x =>
            Error(new IterateeException("Invalid event $x encountered."), Input.Empty)
        }
      }

      step(new java.lang.StringBuilder(100), IndexedSeq())
    }

  }
}
