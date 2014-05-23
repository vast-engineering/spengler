//COMMENTED OUT FOR NOW - add back in when we have an incremental JSON tokenizer

//package com.vast.xml
//
//import play.api.libs.json._
//
//import com.vast.util.iteratee._
//import com.vast.xml.TreeParser.Node
//import com.typesafe.scalalogging.slf4j.LazyLogging
//
///**
// * A set of [[com.vast.util.iteratee.Iteratee]]s and [[com.vast.util.iteratee.Enumeratee]]s that can
// * take an XML document and transform it into a series of play-json JsValue instances.
// *
// * The translation isn't perfect, since the two concepts don't match up exactly - for example, attributes in the source
// * XML don't map perfectly, but some guesses are made as to how to turn attributes into JsValues.
// *
// * @author David Pratt (dpratt@vast.com)
// */
//object JsValueParser extends LazyLogging {
//
//  import Iteratees._
//  import ObjectParser._
//
//  import scala.language.implicitConversions
//
//  /**
//   * An iteratee that produces the text contents of an XML element. This assumes that the element has
//   * no children, and will produce an error if anything but Characters or CData is encountered before an EndElement
//   * event.
//   */
//  def jsonString: Iteratee[XMLEvent, JsValue] = textOnly.map(_.map(JsString).getOrElse(JsNull))
//
//  /**
//   * Attempt to parse the contents of a text-only XML element as a JsNumber. If the text contents are not
//   * parseable as a [[scala.math.BigDecimal]], an error is signaled.
//   */
//  def jsonNumber: Iteratee[XMLEvent, JsValue] = textOnly.flatMap { textOpt =>
//    try {
//      Done(textOpt.map(x => JsNumber(BigDecimal(x))).getOrElse(JsNull))
//    } catch {
//      case e: NumberFormatException => Error(new IterateeException(s"Could not parse a number from the input $textOpt", e), Input.Empty)
//    }
//  }
//
//  /**
//   * Attempt to parse a tree of XML elements into a JsObject. Child elements will be added as fields in the object with the following rules
//   *
//   *  - If the child element is text-only, attempt to parse it as a JsString value
//   *  - If the child element is text-only but contains no text, it is parsed as a JsNull
//   *  - If an element of a given name appears more than once as a child, those child elements
//   *    are collected up and parsed as a JsArray value.
//   */
//  def jsonObject: Iteratee[XMLEvent, JsObject] = jsObject(name => Some(jsValue))
//
//  private[this] def jsonObjectCreator: Iteratee[(String, JsValue), JsObject] = Iteratee.getChunks.map { children =>
//    new JsObject(transformChildren(children))
//  }
//
//  private[this] def transformChildren(children: Seq[(String, JsValue)]): Seq[(String, JsValue)] = {
//    //XML doesn't define explicit arrays - we need to aggregate all the elements of the same name into JsArray values
//    //transform to a List[String, List[JsValue]]
//    val valuesList: Seq[(String, Seq[JsValue])] = children.groupBy(tuple => tuple._1).mapValues(tupleList => tupleList.map(_._2)).toSeq
//
//    //now reduce the List[JsValue] values down
//    //if the list is
//    valuesList.collect {
//      case (name, x) if x.size <= 1 =>
//        name -> x.head
//      case (name, x) =>
//        //we have a JsArray
//        name -> JsArray(x)
//    }
//  }
//
//  private def jsValue: Iteratee[XMLEvent, JsValue] = TreeParser.parseTree.map(node2jsValue)
//
//  def jsObject(valueParsers: (String, Iteratee[XMLEvent, JsValue])*): Iteratee[XMLEvent, JsObject] =
//    jsObject(Map(valueParsers: _*))
//
//  /**
//   * Iteratee for an XML element.  Adapts a stream of XMLEvents into a result value. Child elements are parsed
//   * using the given mappings.
//   *
//   */
//  def jsObject(parserMap: Map[String, Iteratee[XMLEvent, JsValue]]): Iteratee[XMLEvent, JsObject] =
//    jsObject { (key: String) =>
//      parserMap.get(key)
//    }
//
//  /**
//   * Iteratee for an XML element.  Adapts a stream of XMLEvents into a result value. Child elements are parsed
//   * using the given mappings.
//   *
//   */
//  def jsObject(valueHandlerProducer: String => Option[Iteratee[XMLEvent, JsValue]]): Iteratee[XMLEvent, JsObject] = {
//    val elemHandler = (name: String, attrs: Map[String, String]) =>
//      valueHandlerProducer(name).map(it => it.map(x => (name, x)))
//    xmlObject(jsonObjectCreator, elemHandler)
//  }
//
//  private[this] def node2jsValue(node: Node): JsValue = {
//    //a lame attempt to transform XML tree to JsValue
//    //if the node has children, it's a JsObject
//    //if it has text, it's a JsString
//    //else it's JsNull
//    //TODO - maybe put some heuristics or smarts in here
//    //ideas:
//    //  if it's a text-only element and has an attribute named 'type', use that as a hint
//    //  if the text-only value appears numeric, try it as a JsNumber (don't know how to do that efficiently)
//    if(!node.children.isEmpty) {
//      val jsChildren = node.children.map {
//        case (name, value) => name -> node2jsValue(value)
//      }
//      //treat an object node's attributes as JsString children - this is the only really
//      //useful way I can think of to map the concepts
//      val attrChildren = node.attrs.map(x => (x._1, JsString(x._2)))
//      val xformChildren = transformChildren(jsChildren ++ attrChildren)
//      JsObject(xformChildren)
//    } else if(node.text.isDefined) {
//      JsString(node.text.get)
//    } else {
//      JsNull
//    }
//  }
//
//}
