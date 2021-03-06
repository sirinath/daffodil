package edu.illinois.ncsa.daffodil.section05.facets

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

class TestFacetsDebug {
  val testDir = "/edu/illinois/ncsa/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa), validateTDMLFile = false)

  // Note, these won't pass until Decimal is implemented
  @Test def test_totalDigits_Pass_Decimal{ runner.runOneTest("checkTotalDigits_Pass_Decimal") }
  @Test def test_totalDigits_Fail_Decimal{ runner.runOneTest("checkTotalDigits_Fail_Decimal") }
  @Test def test_fractionDigits_Pass{ runner.runOneTest("checkFractionDigits_Pass") }
  @Test def test_fractionDigits_Fail{ runner.runOneTest("checkFractionDigits_Fail") }
  @Test def test_fractionDigits_Pass_LessDigits{ runner.runOneTest("checkFractionDigits_Pass_LessDigits")} 
  @Test def test_totalDigitsAndFractionDigits_Pass { runner.runOneTest("checkTotalDigitsFractionDigits_Pass")}  
  @Test def test_totalDigitsAndFractionDigits_Fail { runner.runOneTest("checkTotalDigitsFractionDigits_Fail")}  

  @Test def test_fractionDigitsPass() { runner.runOneTest("fractionDigitsPass") }
  @Test def test_fractionDigitsFail() { runner.runOneTest("fractionDigitsFail") }
  @Test def test_fractionTotalDigitsPass() { runner.runOneTest("fractionTotalDigitsPass") }
  @Test def test_fractionTotalDigitsFail2() { runner.runOneTest("fractionTotalDigitsFail2") }
  @Test def test_fractionTotalDigitsFail3() { runner.runOneTest("fractionTotalDigitsFail3") }

  @Test def test_totalDigits01() { runner.runOneTest("totalDigits01") }
  @Test def test_totalDigits02() { runner.runOneTest("totalDigits02") }

  @Test def test_totalDigits05b() { runner.runOneTest("totalDigits05b") }

}
