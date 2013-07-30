package com.vast.xml

import com.vast.util.iteratee._
import com.typesafe.scalalogging.slf4j.Logging

/**
 * A set of [[com.vast.util.iteratee.Iteratee]]s that can parse a stream of [[com.vast.xml.XMLEvent]]s into
 * a tree of Node instances.
 *
 */
object TreeParser extends Logging {

  case class Node(name: String, attrs: Map[String, String], children: Seq[(String, Node)], text: Option[String])

  import Iteratees._

  sealed trait XMLNode
  case class XMLDocument(root: XMLElement) extends XMLNode

  case class XMLElement(name: String, attributes: Map[String, String], children: Seq[XMLElement], text: Option[String]) extends XMLNode
  object XMLElement {
    def apply(node: Node): XMLElement = {
      XMLElement(node.name, node.attrs, node.children.map(child => XMLElement(child._2)), node.text)
    }
  }

  def xmlDocument = document(xmlElement).map { root =>
    XMLDocument(root)
  }

  def xmlElement = parseTree.map { node =>
    XMLElement(node)
  }

  def parseTree: Iteratee[XMLEvent, Node] = {
    expectStartElement.flatMap { startElementEvent =>
      def step(text: java.lang.StringBuilder, children: IndexedSeq[(String, Node)]): Iteratee[XMLEvent, Node] = Cont {
        case in@Input.EOF =>
          Error(new IterateeException("Unexpected EOF."), in)
        case in@Input.Empty => step(text, children)
        case in@Input.El(event) => event match {
          //parse text
          case Characters(data) => step(text.append(data), children)
          case CData(data) => step(text.append(data), children)
          case startElem@StartElement(name, attrs) => parseTree.feed(Input.El(startElem)).flatMap { child =>
            step(text, children :+ (child.name, child))
          }
          case EndElement(name) =>
            if (name != startElementEvent.name) {
              //we got an unexpected end element
              Error(new IterateeException(s"Unexpected EndElement($name) - only EndElement(${startElementEvent.name} is valid in this position."), Input.Empty)
            } else {
              val rawText = text.toString.trim
              Done(Node(startElementEvent.name, startElementEvent.attributes, children, if(rawText.isEmpty) None else Some(rawText)))
            }
          case UnknownEvent(eventType) =>
            logger.debug(s"Skipping element of type $eventType")
            step(text, children) //do nothing - skip the element
          case x =>
            Error(new IterateeException("Invalid event $x encountered."), Input.Empty)
        }
      }

      step(new java.lang.StringBuilder(100), IndexedSeq())
    }

  }
}
