package edu.illinois.ncsa.daffodil.xml

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.Implicits._

import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.xml.Node

class TestToJSON extends ToJSON {

  @Test def testRoot1() {
    val xml: Node = <foo>bar</foo>
    val jsonStr = xml.toJson.toString
    assertEquals("""{"foo":"bar"}""", jsonStr)
  }

  @Test def testNest1() {
    val xml: Node = <foo><baz>bar</baz></foo>
    val jsonStr = xml.toJson.toString
    assertEquals("""{"foo":{"baz":"bar"}}""", jsonStr)
  }

  @Test def testEmpty1() {
    val xml: Node = <foo/>
    val jsonStr = xml.toJson.toString
    assertEquals("""{"foo":[]}""", jsonStr)
  }

  @Test def testNil1() {
    val xml: Node = <foo xmlns:xsi={ XMLUtils.XSI_NAMESPACE } xsi:nil="true"/>
    val jsonStr = xml.toJson.toString
    println(jsonStr)
    assertEquals("""{"foo":null}""", jsonStr)
  }

  @Test def testArray1() {
    val xml: Node = <foo><baz>bar</baz><baz>bar</baz><baz>bar</baz><baz>bar</baz></foo>
    val jsonStr = xml.toJson.toString
    println(jsonStr)
    assertEquals("""{"foo":{"baz":["bar","bar","bar","bar"]}}""", jsonStr)
  }

  @Test def testMix1() {
    val xml: Node = <foo xmlns:xsi={ XMLUtils.XSI_NAMESPACE }><baz>bar</baz><baz>bar</baz><quux xsi:nil="true"/><frob/><boo>bar</boo><boo>bar</boo></foo>
    val jsonStr = xml.toJson.toString
    println(jsonStr)
    assertEquals("""{"foo":{"frob":[],"baz":["bar","bar"],"quux":null,"boo":["bar","bar"]}}""", jsonStr)
  }

  @Test def testMix2() {
    val xml: Node = <foo xmlns:xsi={ XMLUtils.XSI_NAMESPACE }><baz>bar</baz><frob/><baz>bar</baz><baz>bar</baz></foo>
    val jsonStr = xml.toJson.toString
    println(jsonStr)
    assertEquals("""{"foo":{"frob":[],"baz":["bar","bar"],"boo":["bar","bar"]}}""", jsonStr)
  }
}