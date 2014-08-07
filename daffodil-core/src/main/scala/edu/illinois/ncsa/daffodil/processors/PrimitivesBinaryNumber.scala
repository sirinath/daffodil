package edu.illinois.ncsa.daffodil.processors

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

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ LengthKind, LengthUnits, ByteOrder, AlignmentUnits }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.parsers.UnsignedRuntimeLengthRuntimeByteOrderBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.UnsignedKnownLengthRuntimeByteOrderBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.SignedRuntimeLengthRuntimeByteOrderBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.SignedKnownLengthRuntimeByteOrderBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryKnownLengthBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryRuntimeLengthBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.FloatKnownLengthRuntimeByteOrderBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.DecimalKnownLengthRuntimeByteOrderBinaryNumberParser
import edu.illinois.ncsa.daffodil.processors.parsers.DoubleKnownLengthRuntimeByteOrderBinaryNumberParser

trait RuntimeExplicitLengthMixin[T] {
  self: Terminal =>
  def e: ElementBase

  // get at compile time, not runtime.
  val lUnits = e.lengthUnits

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getBitLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes * toBits)
  }
  def getLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes)
  }
}

trait KnownLengthInBitsMixin[T] {
  self: BinaryNumberBase[T] =>
  def len: Long
  def getBitLength(s: PState) = (s, len) // already in bits, so no multiply by 8 for this one.
}

trait RuntimeExplicitByteOrderMixin[T] {
  self: BinaryNumberBase[T] =>
  def e: ElementBase
  val bo = e.byteOrder // ensure byteOrder compiled expression is computed non lazily at compile time
}

// TODO: Double Conversion as a Sign-Trait

abstract class BinaryNumberBase[T](val e: ElementBase) extends Terminal(e, true) {
  val primName = e.primType.name

  val (staticJByteOrder, label) = {
    if (e.byteOrder.isConstant) {
      val staticByteOrderString = e.byteOrder.constantAsString
      val staticByteOrder = ByteOrder(Found("byteOrder", staticByteOrderString, context), context)
      staticByteOrder match {
        case ByteOrder.BigEndian => (java.nio.ByteOrder.BIG_ENDIAN, "BE")
        case ByteOrder.LittleEndian => (java.nio.ByteOrder.LITTLE_ENDIAN, "LE")
      }
    } else (null, "Runtime")
  }

  //def getNum(t: Number): BigInt
  override def toString = "binary(xs:" + primName + ", " + label + ")"
  val gram = this

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1, GramName.length) }

  def unparser = DummyUnparser
}

class UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] {

  def parser = new UnsignedRuntimeLengthRuntimeByteOrderBinaryNumberParser(bo, lUnits, this, e)
}

class UnsignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] {

  def parser = new UnsignedKnownLengthRuntimeByteOrderBinaryNumberParser(bo, len, this, e)
}

class SignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] {
  def parser = new SignedRuntimeLengthRuntimeByteOrderBinaryNumberParser(bo, lUnits, this, e)
}

class SignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] {
  def parser = new SignedKnownLengthRuntimeByteOrderBinaryNumberParser(bo, len, this, e)
}

// Not needed. No runtime-determined lengths for binary floats.
//class FloatingPointRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
//  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with FloatingPointMixin[T] {
//}

class HexBinaryKnownLengthBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[String](e) {
  override def toString = "hexBinary(xs:" + primName + ", " + label + ")"
  // get at compile time, not runtime.
  val lUnits = e.lengthUnits

  def parser = new HexBinaryKnownLengthBinaryNumberParser(len, this, e)
}

class HexBinaryRuntimeLengthBinaryNumber(e: ElementBase)
  extends BinaryNumberBase[String](e)
  with RuntimeExplicitLengthMixin[String] {

  override def toString = "hexBinary(xs:" + primName + ", " + label + ")"
  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    (s, java.nio.ByteOrder.BIG_ENDIAN)
  }

  def parser = new HexBinaryRuntimeLengthBinaryNumberParser(lUnits, this, e)
}

class FloatKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Float](e)
  with RuntimeExplicitByteOrderMixin[Float]
  with KnownLengthInBitsMixin[Float] {

  def parser = new FloatKnownLengthRuntimeByteOrderBinaryNumberParser(bo, len, this, e)
}

class DoubleKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Double](e)
  with RuntimeExplicitByteOrderMixin[Double]
  with KnownLengthInBitsMixin[Double] {

  def parser = new DoubleKnownLengthRuntimeByteOrderBinaryNumberParser(bo, len, this, e)
}

class DecimalKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[BigDecimal](e)
  with RuntimeExplicitByteOrderMixin[BigDecimal]
  with KnownLengthInBitsMixin[BigDecimal] {

  def parser = new DecimalKnownLengthRuntimeByteOrderBinaryNumberParser(bo, len, this, e)
}

