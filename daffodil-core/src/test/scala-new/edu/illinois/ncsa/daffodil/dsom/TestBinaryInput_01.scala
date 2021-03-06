package edu.illinois.ncsa.daffodil.dsom

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

// Do no harm number 16 of 626 fail in regression, 154 in total of 797

class TestBinaryInput_01 {

  /*** DFDL-334 ***/
  // Verify Bit Extraction

  @Test def testOneBit1() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes("00000011"))
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    val bit1 = inStream.getPartialByte(6, 2, 0)
    assertEquals(3, bit1)
    val bit2 = inStream.getPartialByte(4, 2, 0)
    assertEquals(0, bit2)
  }

  @Test def testOneBit2() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes("11000000"))
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    val bit1 = inStream.getPartialByte(0, 2, 0)
    assertEquals(3, bit1)
    val bit2 = inStream.getPartialByte(2, 2, 0)
    assertEquals(0, bit2)
  }

  @Test def testOneBit3() {
    val in = Misc.byteArrayToReadableByteChannel(Misc.bits2Bytes("00000011"))
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    val (n, len) = inStream.getBitSequence(6, 2, java.nio.ByteOrder.BIG_ENDIAN)
    assertEquals(BigInt(3), n)
    assertEquals(8, len)
    //    val bit2 = inStream.getPartialByte(4, 2, 0)
    //    assertEquals(0, bit2)
  }

  @Test
  def testBufferBitExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getPartialByte(1, 3) == 3)
  }

  @Test
  def testBufferBitExtractionShift() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getPartialByte(1, 3, 2) == 12)
  }

  @Test
  def testBufferLeastSignificantBitExtractionShift() {
    var in = Misc.stringToReadableByteChannel("4")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getPartialByte(5, 3, 2) == 16)
  }

  // Verify aligned byte/short/int/long/bigint extraction
  @Test
  def testBufferByteBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(0, 8, java.nio.ByteOrder.BIG_ENDIAN)._1 == 51)
  }

  @Test
  def testBufferByteLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(0, 8, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 51)
  }

  @Test
  def testBufferShortBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Om")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(0, 16, java.nio.ByteOrder.BIG_ENDIAN)._1 == 20333)
  }

  @Test
  def testBufferShortLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Om")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(0, 16, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 27983)
  }

  @Test
  def testBufferIntBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Help")
    val inStream = InStream.fromByteChannel(null, in, 4, -1)
    assertTrue(inStream.getBitSequence(0, 32, java.nio.ByteOrder.BIG_ENDIAN)._1 == 1214606448)
  }

  @Test
  def testBufferIntLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Help")
    val inStream = InStream.fromByteChannel(null, in, 4, -1)
    assertTrue(inStream.getBitSequence(0, 32, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 1886152008)
  }

  @Test
  def testBufferLongBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Harrison")
    val inStream = InStream.fromByteChannel(null, in, 8, -1)
    assertTrue(inStream.getBitSequence(0, 64, java.nio.ByteOrder.BIG_ENDIAN)._1.toString ==
      "5215575679192756078")
  }

  @Test
  def testBufferLongLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Harrison")
    val inStream = InStream.fromByteChannel(null, in, 8, -1)
    assertTrue(inStream.getBitSequence(0, 64, java.nio.ByteOrder.LITTLE_ENDIAN)._1.toString ==
      "7957705963315814728")
  }

  @Test
  def testBufferBigIntBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Something in the way she moves, ")
    val inStream = InStream.fromByteChannel(null, in, 32, -1)
    assertTrue(inStream.getBitSequence(0, 256, java.nio.ByteOrder.BIG_ENDIAN)._1.toString ==
      "37738841482167102822784581157237036764884875846207476558974346160344516471840")
  }

  @Test
  def testBufferBigIntLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("Something in the way she moves, ")
    val inStream = InStream.fromByteChannel(null, in, 32, -1)
    assertTrue(inStream.getBitSequence(0, 256, java.nio.ByteOrder.LITTLE_ENDIAN)._1.toString ==
      "14552548861771956163454220823873430243364312915206513831353612029437431082835")
  }

  // Aligned but not full string
  @Test
  def testBufferPartialIntBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 24, java.nio.ByteOrder.BIG_ENDIAN)._1 == 5456468)
  }

  @Test
  def testBufferPartialIntLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 24, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 5522003)
  }

  // Non-Aligned 1 Byte or less
  @Test
  def testBufferBitNumberBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(1, 3, java.nio.ByteOrder.BIG_ENDIAN)._1 == 3)
  }

  @Test
  def testBufferBitNumberLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3")
    val inStream = InStream.fromByteChannel(null, in, 1, -1)
    assertTrue(inStream.getBitSequence(1, 3, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 3)
  }

  @Test
  def testBufferBitByteBigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3>")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(2, 8, java.nio.ByteOrder.BIG_ENDIAN)._1 == 204)
  }

  @Test
  def testBufferBitByteLittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("3>")
    val inStream = InStream.fromByteChannel(null, in, 2, -1)
    assertTrue(inStream.getBitSequence(2, 8, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 204)
  }

  // Non-Aligned multi-byte
  @Test
  def testBufferPartialInt22At0BigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 22, java.nio.ByteOrder.BIG_ENDIAN)._1 == 1364117)
  }

  @Test
  def testBufferPartialInt22At0LittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(0, 22, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 1393235)
  }

  @Test
  def testBufferPartialInt22At2BigEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(2, 22, java.nio.ByteOrder.BIG_ENDIAN)._1 == 1262164)
  }

  @Test
  def testBufferPartialInt22At2LittleEndianExtraction() {
    var in = Misc.stringToReadableByteChannel("SBT")
    val inStream = InStream.fromByteChannel(null, in, 3, -1)
    assertTrue(inStream.getBitSequence(2, 22, java.nio.ByteOrder.LITTLE_ENDIAN)._1 == 1313101)
  }

}
