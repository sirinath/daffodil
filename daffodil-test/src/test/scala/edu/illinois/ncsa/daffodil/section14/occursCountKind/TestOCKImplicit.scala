package edu.illinois.ncsa.daffodil.section14.occursCountKind

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

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.util.Misc

import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite

class TestOCKImplicit {
  val testDir = "/edu/illinois/ncsa/daffodil/section14/occursCountKind/"
  val aa = testDir + "ockImplicit.tdml"
  val res = Misc.getRequiredResource(aa)
  println(res)
  lazy val runner = new DFDLTestSuite(res)

  @Test def test_ockImplicit1() { runner.runOneTest("ockImplicit1") }
  // Waiting on DFDL-703 for clearer error message
  // @Test def test_ockImplicit2() { runner.runOneTest("ockImplicit2") }
  @Test def test_ockImplicit3() { runner.runOneTest("ockImplicit3") }
  // Waiting on DFDL-703 for clearer error message
  // @Test def test_ockImplicit4() { runner.runOneTest("ockImplicit4") }
}
