package edu.illinois.ncsa.daffodil.section08.property_scoping

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
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestPropertyScoping {
  val testDir = "/edu/illinois/ncsa/daffodil/section08/property_scoping/"
  val aa = testDir + "PropertyScoping.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_property_scoping_01() { runner.runOneTest("property_scoping_01") }
  @Test def test_property_scoping_06() { runner.runOneTest("property_scoping_06") }
  @Test def test_group_ref() { runner.runOneTest("group_ref") }

  // Moved back to debug, because validation is no longer detecting the error it is
  // expecting
  // @Test def test_property_shortFormSchemaFail() { runner.runOneTest("shortFormSchemaFail") }

  @Test def test_format_nesting_01() { runner.runOneTest("format_nesting_01") }

  val tdml = testDir + "PropertyScoping_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_property_scoping_02() { runner_01.runOneTest("property_scoping_02") }
  @Test def test_property_scoping_03() = { runner_01.runOneTest("property_scoping_03") }
  @Test def test_property_scoping_04() { runner_01.runOneTest("property_scoping_04") }
  @Test def test_property_scoping_05() { runner_01.runOneTest("property_scoping_05") }
  @Test def test_property_scoping_07() { runner_01.runOneTest("property_scoping_07") }
  @Test def test_property_scoping_08() { runner_01.runOneTest("property_scoping_08") }
  @Test def test_property_scoping_09() { runner_01.runOneTest("property_scoping_09") }
  @Test def test_property_scoping_10() { runner_01.runOneTest("property_scoping_10") }
  @Test def test_property_scoping_11() { runner_01.runOneTest("property_scoping_11") }
  @Test def test_NearestEnclosingSequenceElementRef() { runner_01.runOneTest("NearestEnclosingSequenceElementRef") }

  @Test def test_property_refElementFormFail() = { runner_01.runOneTest("refElementFormFail") }

}
