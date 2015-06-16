/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Misc._
import edu.illinois.ncsa.daffodil.util.Bits
import java.io.ByteArrayInputStream
import java.nio.charset.Charset
import java.nio.CharBuffer
import java.io.InputStreamReader
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import java.io.InputStream
import java.io.File
import org.apache.commons.io.IOUtils
import java.nio.file.Files
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.OutStream
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.io.Utils
import edu.illinois.ncsa.daffodil.io.Dump
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.io.DataInputStream
import java.nio.channels.Channels
import java.io.ByteArrayOutputStream
import edu.illinois.ncsa.daffodil.processors.unparsers.GeneralOutStream
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder

@deprecated("2015-06-22", "Should use DataInputStreamFactory")
object InStreamFactory {

  val mandatoryAlignment = 8

  @deprecated("2015-06-22", "Should use DataInputStreamFactory")
  def fromByteChannel(context: ElementRuntimeData,
    in: DFDL.Input,
    bitStartPos0b: Long,
    numBitsLimit: Long, // a count, not a position
    bitOrder: BitOrder) = {
    val dis = ByteBufferDataInputStream(in, bitStartPos0b)
    if (numBitsLimit > 0) dis.setBitLimit0b(One(bitStartPos0b + numBitsLimit))
    dis.setBitOrder(bitOrder)
    new InStream(context, dis)
  }

}

/**
 * Temporary backward compatibility layer. Allows existing parsers to continue to run
 * even though they still use the older InStream interface, until such time as they are
 * upgraded to DataInputStream.
 */
@deprecated("2015-06-22", "Should use DataInputStream")
case class InStream(
  var context: RuntimeData,
  var dis: DataInputStream)
  extends Logging
  with WithParseErrorThrowing {

  def dataInputStream = dis

  def bitLimit0b: Long = dataInputStream.bitLimit0b.getOrElse(-1)
  def bitPos0b: Long = dataInputStream.bitPos0b
  def charLimit0b: Long = -1
  def reader: edu.illinois.ncsa.daffodil.util.Maybe[DFDLCharReader] = Nope

  def setDebugging(b: Boolean) {
    dataInputStream.setDebugging(b)
  }

  // just keep a private dataInputStream because it has the access to pastData and futureData
  // that we want for making dumps.

  def assignFrom(other: InStream) {
    val oth = other.asInstanceOf[InStream]
    context = oth.context
    dis = oth.dis
  }

  def duplicate() = {
    val c = copy(dis = dis.asInstanceOf[ByteBufferDataInputStream].makeACopy())
    c
  }
  // Let's eliminate duplicate information between charPos of the PState, the
  // InStream, and the Reader. It's the reader, and only the reader.
  def charPos0b = reader.map { _.characterPos }.getOrElse(-1).toLong

  /**
   * withPos changes the bit position of the stream, and maintains the char reader
   * which is available to decode characters at that position.
   *
   * It is critical to performance that the reader be preserved if it can be. That is, if we are
   * moving through characters of text in the same encoding, with no binary data or alignment going on, then
   * we *must* retain the reader. Creating a new reader has high overhead in that as soon as you create one and
   * read anything from it, it will read-ahead a large block of characters. If every element was creating
   * a new reader, we'd be reading data over and over again.
   *
   * So it is NOT ok to just pass None as the third argument. Only do that if you have
   * just been handling binary data, or just did an alignmentFill that really inserted some bits.
   *
   * It is well worth it to test and branch to preserve the reader. E.g., AlignmentFill should not
   * create a new reader unless it actually moved over some number of bits. If the alignment is 1 (bit),
   * or the actual amount of alignment fill to be skipped in a particular data stream is 0, then
   * one should preserve the reader.
   */
  def withPos(newBitPos0b: Long, newCharPos0b: Long, newReader: Maybe[DFDLCharReader]): InStream = {
    dataInputStream.asInstanceOf[ByteBufferDataInputStream].notifyNewBitPos0b(newBitPos0b)
    this
  }

  def withPos(newBitPos0b: Long, newCharPos0b: Long): InStream = {
    dataInputStream.asInstanceOf[ByteBufferDataInputStream].notifyNewBitPos0b(newBitPos0b)
    this
  }

  def withEndBitLimit(newBitLimit0b: Long): InStream = {
    dataInputStream.setBitLimit0b(One(newBitLimit0b))
    this
  }

  /**
   * changes the bitOrder - must be done at a byte boundary.
   */
  def withBitOrder(bitOrder: BitOrder) = {
    Assert.usage(dataInputStream.bitPos0b % 8 == 0)
    dataInputStream.setBitOrder(bitOrder)
    this
  }

  def getCharReader(charset: Charset, bitPos0b: Long): DFDLCharReader = {
    dataInputStream.setDecoder(charset.newDecoder()) // Performance: belch. Allocate a new Decoder for every time we read?
    DFDLCharReaderFromDataInputStream(this)
  }

  def getBytes(bitPos: Long, numBytes: Long): Array[Byte] = {
    val bb = ByteBuffer.allocate(numBytes.toInt)
    val savedBitPos = dataInputStream.bitPos0b
    var maybeNBytes: Maybe[Int] = One(0)
    var total: Int = 0
    while (maybeNBytes.isDefined && total < numBytes) {
      maybeNBytes = dataInputStream.fillByteBuffer(bb)
      if (maybeNBytes.isDefined) {
        total += maybeNBytes.get
      }
    }
    withPos(savedBitPos, -1)
    if (total != numBytes) throw new IndexOutOfBoundsException("numBytes=" + numBytes)
    val bytes = bb.array()
    bytes
  }

  private def setByteAndBitOrder(byteOrd: java.nio.ByteOrder, bitOrd: BitOrder) {
    byteOrd match {
      case java.nio.ByteOrder.BIG_ENDIAN => dataInputStream.setByteOrder(ByteOrder.BigEndian)
      case java.nio.ByteOrder.LITTLE_ENDIAN => dataInputStream.setByteOrder(ByteOrder.LittleEndian)
    }
    dataInputStream.setBitOrder(bitOrd)
  }

  def getLong(bitPos0b: Long, bitCount: Long, byteOrd: java.nio.ByteOrder, bitOrd: BitOrder): Long = {
    Assert.usage(bitCount <= 63)
    this.withPos(bitPos0b, -1)
    setByteAndBitOrder(byteOrd, bitOrd)
    val maybeLong = dataInputStream.getUnsignedLong(bitCount.toInt)
    if (maybeLong.isEmpty) {
      throw new IndexOutOfBoundsException("bitStart: %s bitLength: %s".format(bitPos0b, bitCount))
    }
    maybeLong.get.toLong
  }

  /**
   * Constructs a BigInt from the data stream
   */
  def getBigInt(bitPos0b: Long, bitCount: Long, byteOrd: java.nio.ByteOrder, bitOrd: BitOrder): BigInt = {
    setByteAndBitOrder(byteOrd, bitOrd)
    val savedBitPos = this.bitPos0b
    try {
      withPos(bitPos0b, -1)
      val maybeBigInt = dataInputStream.getUnsignedBigInt(bitCount.toInt)
      if (maybeBigInt.isEmpty) {
        throw new IndexOutOfBoundsException("bitStart: %s bitLength: %s".format(bitPos0b, bitCount))
      }
      maybeBigInt.get
    } finally {
      withPos(savedBitPos, -1)
    }
  }

  // This still requires alignment, and that a whole byte is available

  def getByte(bitPos: Long, byteOrder: java.nio.ByteOrder, bitOrder: BitOrder) = {
    setByteAndBitOrder(byteOrder, bitOrder)
    val maybeLong = dataInputStream.getSignedLong(8)
    if (maybeLong.isEmpty) {
      throw new IndexOutOfBoundsException("bitStart: %s bitLength: %s".format(bitPos0b, 8))
    }
    maybeLong.get.toByte
  }

  def getRawByte(bitPos: Long, byteOrder: java.nio.ByteOrder, bitOrder: BitOrder) = {
    Assert.usage(bitPos % 8 == 0)
    getByte(bitPos, byteOrder, bitOrder)
  }

  /**
   * Calling this forces the entire input into memory.
   */
  def lengthInBytes: Long = {
    Assert.invariant(dataInputStream.bitLimit0b.isDefined)
    dataInputStream.bitLimit0b.get / 8
  }

}
