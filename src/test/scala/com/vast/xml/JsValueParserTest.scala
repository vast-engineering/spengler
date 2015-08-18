//package com.vast.xml
//
//import org.scalatest.WordSpec
//import com.typesafe.scalalogging.LazyLogging
//
//import play.api.libs.json._
//import play.api.libs.functional.syntax._
//
///**
// *
// * @author David Pratt (dpratt@vast.com)
// */
//class JsValueParserTest extends WordSpec with AsyncSupport with LazyLogging {
//
//  import JsValueParser._
//  import Iteratees._
//
//  "The XML Parser" should {
//
//    val goodBytes =
//      """
//        |<simpleObject>
//        | <stringField>foo</stringField>
//        | <intField>1</intField>
//        | <doubleField>3.14</doubleField>
//        |</simpleObject>
//      """.stripMargin.getBytes
//
//    val objectIt = document(jsObject(
//      "stringField" -> jsonString,
//      "intField" -> jsonNumber,
//      "doubleField" -> jsonNumber
//    ))
//
//    "parse an XML object" in {
//      val expected = Json.toJson(
//        Map(
//          "stringField" -> Json.toJson("foo"),
//          "intField" -> Json.toJson(1),
//          "doubleField" -> Json.toJson(3.14)
//        )
//      )
//
//      expectResult(expected) {
//        blockOnResult(asyncFeedInput(goodBytes, objectIt))
//      }
//    }
//
//    "treat attributes as fields" in {
//      val bytes =
//        """
//          |<simpleObject>
//          | <stringField>foo</stringField>
//          | <intField>1</intField>
//          | <doubleField>3.14</doubleField>
//          |</simpleObject>
//        """.stripMargin.getBytes
//
//      val expected = Json.toJson(
//        Map(
//          "stringField" -> Json.toJson("foo"),
//          "intField" -> Json.toJson(1),
//          "doubleField" -> Json.toJson(3.14)
//        )
//      )
//
//      expectResult(expected) {
//        blockOnResult(asyncFeedInput(bytes, objectIt))
//      }
//    }
//
//    "parse a case class" in {
//      case class TestClass(stringField: String, intField: Int, doubleField: Double)
//      implicit val testClassReads: Reads[TestClass] = (
//        (__ \ "stringField").read[String] and
//          (__ \ "intField").read[Int] and
//          (__ \ "doubleField").read[Double]
//        )(TestClass)
//
//      val caseIt = objectIt.map { xObject =>
//        xObject.validate[TestClass]
//      }
//
//      expectResult(TestClass("foo", 1, 3.14)) {
//        blockOnResult(asyncFeedInput(goodBytes, caseIt)).fold(
//          errors => fail(s"Could not parse XML into object due to errors - $errors"),
//          value => value
//        )
//      }
//    }
//
//  }
//
//
//}
