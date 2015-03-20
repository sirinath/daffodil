package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.Implicits._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.util.Misc
import junit.framework.Assert._
import java.io.FileOutputStream
import java.nio.channels.WritableByteChannel
import java.io.FileWriter
import java.io.File
import java.nio.ByteBuffer
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestInfosetJSON {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val ex = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testXMLToInfoset1() {
    val testSchema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element minOccurs="0" maxOccurs="unbounded" name="a" type="xs:string" dfdl:length="1" dfdl:lengthKind="explicit" dfdl:occursCountKind="expression" dfdl:occursCount="{ ../c }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    try {
      // Debugger.setDebugging(true)
      val res = TestUtils.testString(testSchema, "2AB")
      val xml = res.result
      TestUtils.assertEqualsXMLElements(<b><c>2</c><a>A</a><a>B</a></b>, xml)
      res.result
    } finally {
      // Debugger.setDebugging(false)
    }

  }
}