package com.vast.xml

import org.scalatest.WordSpec
import com.typesafe.scalalogging.slf4j.LazyLogging

class NodeSeqParserTest extends WordSpec with AsyncSupport with LazyLogging {

  import NodeSeqParser._

  val goodBytes =
    """
      |<simpleDoc a="b">
      | <firstField firstAttr="firstValue" secondAttr="secondValue">foo</firstField>
      | <secondField thirdAttr="thirdValue">secondValue</secondField>
      |</simpleDoc>
    """.stripMargin.getBytes

  "The NodeSeq parser" should {
    "correctly parse XML" in {
      val it = parseDocument
      val res = blockOnResult(asyncFeedInput(goodBytes, it))
      Console.println(res)
    }
  }

}
