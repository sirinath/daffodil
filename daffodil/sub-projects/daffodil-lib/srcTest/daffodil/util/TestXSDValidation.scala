package daffodil.util
import scala.xml._ ;import junit.framework.Assert._ ;import org.scalatest.junit.JUnit3Suite ;import java.io.StringReader ;import java.io.InputStreamReader ;class TestXSDValidation extends JUnit3Suite {     val xmlnsURI = "http://www.w3.org/2001/XMLSchema" ;  val xsdSubsetURI = "http://www.ogf.org/dfdl/dfdl-1.0/XMLSchemaSubset" ;  val dfdlURI = "http://www.ogf.org/dfdl/dfdl-1.0/" ;  val xsiURI = "http://www.w3.org/2001/XMLSchema-instance" ;   val targetNS = "http://example.com" ;    def testValidationNoTargetNamespace() {    val schema =        <xs:schema xmlns:xs={ xmlnsURI }>         <xs:element name="foo" type="xs:string"/>      </xs:schema>       ; // xml literals confuse Eclipse's scala plug in. Put them in to make it happy.      val document =         <foo>bar</foo> ;      Validator.validateXMLNodes(schema, document)  }  ;  def testValidationWithTargetNamespace() {    val schema =        <xs:schema xmlns:xs={ xmlnsURI } targetNamespace={ targetNS }>         <xs:element name="foo" type="xs:string"/>      </xs:schema>       ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.      val document =         <foo xmlns={ targetNS }>bar</foo> ;      Validator.validateXMLNodes(schema, document)  }   ;  def testSchemaValidationQualifiedXSPrefixes() {      ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.      val document =       <xs:schema xmlns:xs={ xmlnsURI } targetNamespace={ targetNS }>         <xs:element name="foo" type="xs:string"/>      </xs:schema> ;      val documentReader = new StringReader(document.toString()) ;      val schemaStream = Validator.daffodilLibSchema("src/xsd/XMLSchema.xsd") ;      val schemaReader = new InputStreamReader(schemaStream) ;      Validator.validateXMLStream(schemaReader, documentReader)  }   ;  def testSchemaValidationNoQualifiedXSPrefixes() {      ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.      val document =       <schema xmlns={ xmlnsURI } xmlns:xs={ xmlnsURI } targetNamespace={ targetNS }>         <element name="foo" type="xs:string"/>      </schema> ;      val documentReader = new StringReader(document.toString()) ;      val schemaStream = Validator.daffodilLibSchema("src/xsd/XMLSchema.xsd") ;      val schemaReader = new InputStreamReader(schemaStream) ;      Validator.validateXMLStream(schemaReader, documentReader)  }     ;  def testSchemaValidationNoTargetNamespaceAndNoQualifiedXSPrefixes() {      ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.      val document =       <schema xmlns={ xmlnsURI } xmlns:xs={ xmlnsURI }>         <element name="foo" type="xs:string"/>      </schema> ;      val documentReader = new StringReader(document.toString()) ;      val schemaStream = Validator.daffodilLibSchema("src/xsd/XMLSchema.xsd") ;      val schemaReader = new InputStreamReader(schemaStream) ;      Validator.validateXMLStream(schemaReader, documentReader)  }  ;  def testInvalidSchemaValidationNoTargetNamespaceAndNoQualifiedXSPrefixes() {      ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.      val document =       <schema xmlns={ xmlnsURI } xmlns:xs={ xmlnsURI }>         <element notValidAttribute="foo" type="xs:string"/>      </schema> ;      val documentReader = new StringReader(document.toString()) ;      val schemaStream = Validator.daffodilLibSchema("src/xsd/XMLSchema.xsd") ;      val schemaReader = new InputStreamReader(schemaStream) ;      val ex = intercept[Exception] {         Validator.validateXMLStream(schemaReader, documentReader)      };      val msg = ex.getMessage();      val hasMsgInfo = msg.contains("notValidAttribute");      assertTrue(hasMsgInfo);  }  ;  /**   * constructs a DFDL schema using our defined namespaces including for the subset of XSD we support in DFDL.   */  def xsdUsingSubset(topLevelAnnotations: Seq[Node], contentElements: Seq[Node]) = {    val sch =       <xs:schema xmlns:xs={ xsdSubsetURI } xmlns:dfdl={ dfdlURI }  xmlns:xsi={ xsiURI } targetNamespace= { targetNS }>        <xs:annotation>          <xs:appinfo source={ dfdlURI }>            { topLevelAnnotations }          </xs:appinfo>        </xs:annotation>        { contentElements }      </xs:schema>       ; // xml literals confuse Eclipse's scala plug in. Put them in to make it happy.                  val schTxt = sch.toString() ;    // we do this to get the namespace prefixes in the argument nodes re-interpreted according to our definitions.    // this is a function of scala's parsing, so things have to be lexically enclosed, which is painful at best.     // I'd really prefer to be able to do some sort of withMyNSByDefault { <foo:bar baz:quux="value"/> }    // but I don't know how to achieve that.    val sch2 = XML.loadString(schTxt) ;    sch2  } ;    /**   * constructs a DFDL schema using our defined namespaces including for the subset of XSD we support in DFDL.   * But this variation does not xs: qualify all the XML Schema elements. Many DFDL schemas will get written   * this way, even though it is a sort of frowned-upon style.   */  def xsdUsingSubsetNoPrefix(topLevelAnnotations: Seq[Node], contentElements: Seq[Node]) = {    val sch =       <schema xmlns={ xsdSubsetURI } xmlns:xs={ xsdSubsetURI } xmlns:dfdl={ dfdlURI }  xmlns:xsi={ xsiURI } targetNamespace= { targetNS }>        <annotation>          <appinfo source={ dfdlURI }>            { topLevelAnnotations }          </appinfo>        </annotation>        { contentElements }      </schema>       ; // xml literals confuse Eclipse's scala plug in. Put them in to make it happy.                  val schTxt = sch.toString() ;    // we do this to get the namespace prefixes in the argument nodes re-interpreted according to our definitions.    // this is a function of scala's parsing, so things have to be lexically enclosed, which is painful at best.     // I'd really prefer to be able to do some sort of withMyNSByDefault { <foo:bar baz:quux="value"/> }    // but I don't know how to achieve that.    val sch2 = XML.loadString(schTxt) ;    sch2  } ;     /**   * get the implicit built-in XSD file   *    * Takes care of using the resource built-in to the jar, or    * if we're just running interactively in eclipse, doesn't use the jar.   */  ;  def getInternalXSDStream () = Validator.daffodilLibSchema(Validator.dfdlSchemaFileName())  ;  def validateDFDLSchema(xsSchema : NodeSeq) {    val schemaForDFDLSchema = XML.load(getInternalXSDStream) ;    Validator.validateXMLNodes(schemaForDFDLSchema, xsSchema)  }    ;  def testBasicSchemaProcessing() {    val schema =      xsdUsingSubset(Nil,      <xs:element name="foobar" type="xs:int"/>      )      ;    validateDFDLSchema(schema)  }  ;  def testBasicInvalidSchemaProcessing() {    val schema =      xsdUsingSubset(Nil,      <xs:element notValidAttribute="foobar" type="xs:int"/>      )      ;      val ex = intercept[Exception] {         validateDFDLSchema(schema)      };      val msg = ex.getMessage();      val hasMsgInfo = msg.contains("notValidAttribute");      assertTrue(hasMsgInfo);  }  ;  def testBasicSchemaProcessingNoPrefix () {    val schema =      xsdUsingSubsetNoPrefix(Nil,      <element name="foobar" type="xs:int"/>      )      ;    validateDFDLSchema(schema)  }
;
  def testValidationOfXSDSchemaSubsetErrors () {
    val schema1 = xsdUsingSubset(Nil, 
      <xs:element1 name="foo" type="xs:int"/>  // <!-- error. No such element -->
    )
    val ex = intercept[Exception] {
        validateDFDLSchema(schema1)
    }
    println(ex)
    val msg = ex.getMessage()
    val hasElement1InMsg = msg.contains("element1");
    assertTrue(hasElement1InMsg);
  }

  def testValidationOfDFDLLongFormAnnotationPropertyNameErrors() {
    val schema2 = xsdUsingSubset(          <dfdl:defineFormat name="foo">            <dfdl:format byteOrder1="bigEndian"/> <!-- error: incorrect property name -->          </dfdl:defineFormat>,           Nil)
    val ex = intercept[Exception] {       validateDFDLSchema(schema2);       }
    // should throw a validation error. 
    println(ex)
    val msg = ex.getMessage()
    val hasErrorText = msg.contains("byteOrder1")
    assertTrue(hasErrorText)
  }   ;  def testValidationOfDFDLShortFormPropertyValueError() {    val schema2 = xsdUsingSubset(Nil,        <xs:element name="foo" dfdl:byteOrder="invalidValue" type="xs:int"/>);    val ex = intercept[Exception] {       validateDFDLSchema(schema2);       } ;    // should throw a validation error.     println(ex) ;    val msg = ex.getMessage();    val hasErrorText = msg.contains("invalidValue");    assertTrue(hasErrorText)  } 


} // end class