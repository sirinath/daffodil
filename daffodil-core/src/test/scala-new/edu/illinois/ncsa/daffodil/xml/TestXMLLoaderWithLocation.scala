package edu.illinois.ncsa.daffodil.xml

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._
import java.net.URL
import java.io.File

class TestXMLLoaderWithLocation {

  @Test def testBasic() {
    val node = (new DaffodilXMLLoader(BasicStderrErrorHandler)).loadString("<a><b/></a>")
    println(node)
    assertTrue(node.toString.toLowerCase.contains("dafint:line"))
  }

  @Test def testFile1() {
    val tmpXMLFileName = getClass.getName() + ".xml"
    // Our loader looks for xs:schema node, and appends a file attribute
    // if it can.
    val testXML = <xs:schema xmlns:xs={ XMLUtils.XSD_NAMESPACE }><b/></xs:schema>
    try {
      using(new java.io.FileWriter(tmpXMLFileName)) {
        fw =>
          fw.write(testXML.toString())
      }
      val res = new File(tmpXMLFileName)
      val node = (new DaffodilXMLLoader(BasicStderrErrorHandler)).loadFile(res)
      assertTrue(node.toString.toLowerCase.contains("dafint:file"))
    } finally {
      val t = new java.io.File(tmpXMLFileName)
      t.delete()
    }
  }

  @Test def testCatalogResolver() {
    val baseURI: String = new File(".").toURI().toString
    val ldr = new DaffodilXMLLoader(BasicStderrErrorHandler)
    val pId: String = null
    val sId: String = null
    val resolved = DaffodilCatalogResolver.resolver.resolveResource(XMLUtils.XSD_NAMESPACE, XMLUtils.XSD_NAMESPACE, pId, sId, baseURI)
    println(resolved)
  }

  @Test def testFileValidation() {
    val tmpXMLFileName = getClass.getName() + ".xml"
    // Our loader looks for xs:schema node, and appends a file attribute
    // if it can.
    val testXML = <xs:schema xmlns:xs={ XMLUtils.XSD_NAMESPACE }><xs:illegal/></xs:schema>
    try {
      using(new java.io.FileWriter(tmpXMLFileName)) {
        fw =>
          fw.write(testXML.toString())
      }
      val res = new File(tmpXMLFileName)
      val node = (new DaffodilXMLLoader(BasicStderrErrorHandler)).loadFile(res)
      assertTrue(node.toString.toLowerCase.contains("dafint:file"))
    } finally {
      val t = new java.io.File(tmpXMLFileName)
      t.delete()
    }
  }
}