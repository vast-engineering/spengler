package com.vast.xml

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.WordSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import javax.xml.stream.XMLStreamException
import org.apache.commons.io.IOUtils

/**
 *
 * @author David Pratt (dpratt@vast.com)
 */
class TreeParserTest extends WordSpec with AsyncSupport with LazyLogging {

  import TreeParser._

  val goodBytes =
    """
      |<simpleDoc a="b">
      | <firstField firstAttr="firstValue" secondAttr="secondValue">foo</firstField>
      | <secondField thirdAttr="thirdValue">secondValue</secondField>
      |</simpleDoc>
    """.stripMargin.getBytes

  val badBytes =
    """
      |<simpleDoc a="b">
      | <firstField firstAttr="firstValue" secondAttr="secondValue">foo</firstField>
      | <secondField thirdAttr="thirdValue">secondValue</1secondField>
      |</simpleDoc>
    """.stripMargin.getBytes

  val goodBytesParsed = XMLDocument(
    XMLElement("simpleDoc", Map("a" -> "b"), Seq(
      XMLElement("firstField", Map("firstAttr" -> "firstValue", "secondAttr" -> "secondValue"), Seq(), Some("foo")),
      XMLElement("secondField", Map("thirdAttr" -> "thirdValue"), Seq(), Some("secondValue"))
    ), None)
  )

  "The Async XML parser" should {

    "properly parse XML" in {
      val it = xmlDocument

      expectResult(goodBytesParsed) {
        blockOnResult(asyncFeedInput(goodBytes, it))
      }
    }

    "Properly signal errors for invalid XML" in {
      val it = xmlDocument
      intercept[XMLStreamException] {
        blockOnResult(asyncFeedInput(badBytes, it))
      }
    }
  }

  "The sync XML parser" should {

    "properly parse XML" in {
      val it = xmlDocument
      val parser = InputHandlers.syncParser(it)
      expectResult(goodBytesParsed) {
        blockOnResult(parser(goodBytes))
      }
    }
    "Properly signal errors for invalid XML" in {
      val it = xmlDocument
      val parser = InputHandlers.syncParser(it)
      intercept[XMLStreamException] {
        blockOnResult(parser(badBytes))
      }
    }
    "parse a large XML document" in {
      val bytes = IOUtils.toByteArray(classOf[TreeParserTest].getResourceAsStream("/complexResponse.xml"))
      val parser = InputHandlers.syncParser(xmlDocument)
      blockOnResult(parser(bytes))
    }


  }

  "The async and sync parsers" should {
    "return the same result" in {
      val it = xmlDocument
      val syncParser = InputHandlers.syncParser(it)

      val eventuallyResult = Future.sequence(List[Future[XMLNode]](
        asyncFeedInput(goodBytes, it),
        syncParser(goodBytes)
      )).map { results =>
        assert(results.size === 2)
        val first = results.head
        val second = results.tail.head
        assert(first === second)
      }
      blockOnResult(eventuallyResult)
    }
  }

}
