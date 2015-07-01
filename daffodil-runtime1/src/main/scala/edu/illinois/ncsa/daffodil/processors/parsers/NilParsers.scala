/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.PState
import java.nio.ByteBuffer
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.util.OnStack
import java.util.regex.Matcher
import java.nio.CharBuffer

class LiteralNilPatternParser(
  override val parsingPadChar: Maybe[Char],
  val justificationTrim: TextJustificationType.Type,
  erd: ElementRuntimeData,
  patternString: String,
  eName: String,
  override val nilValues: List[String])
  extends LiteralNilParserBase(erd, eName, nilValues)
  with TextReader
  with NilMatcherMixin {

  lazy val pattern = patternString.r.pattern // imagine a really big expensive pattern to compile.

  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  def parse(start: PState): Unit = {

    log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValues, nilValues.length)

    val bytePos = (start.bitPos >> 3).toInt
    log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, start.bitPos)
    log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

    val dis = start.inStream.dataInputStream

    withMatcher { m =>
      val isMatch = dis.lookingAt(m)

      if (!isMatch) {
        PE(start, "%s - %s - Parse failed. Unable to match literal nil value.", this.toString(), eName)
      } else {

        val rawField = m.group()
        // We have a field, is it empty?
        val field = trimByJustification(rawField)
        val isFieldEmpty = field.length() == 0

        if (isFieldEmpty && isEmptyAllowed) {
          // Valid!
          start.thisElement.setNilled()
        } else if (isFieldEmpty && !isEmptyAllowed) {
          // Fail!
          PE(start, "%s - Empty field found but not allowed!", eName)
        } else if (isFieldNilLiteral(field)) {
          // Contains a nilValue, Success!
          start.thisElement.setNilled()
          log(LogLevel.Debug, "%s - Found %s", eName, rawField)
        } else {
          // Fail!
          PE(start, "%s - Does not contain a nil literal!", eName)
        }
      }
    }
  }
}

class LiteralNilExplicitLengthInCharsParser(
  override val parsingPadChar: Maybe[Char],
  val justificationTrim: TextJustificationType.Type,
  erd: ElementRuntimeData,
  eName: String,
  expr: CompiledExpression,
  override val nilValues: List[String])
  extends LiteralNilParserBase(erd, eName, nilValues)
  with TextReader
  with NilMatcherMixin {

  val exprText = expr.prettyExpr

  def parse(start: PState): Unit = {

    val nCharsAsAny = expr.evaluate(start)
    val nChars = AsIntConverters.asLong(nCharsAsAny) //nBytesAsAny.asInstanceOf[Long]
    log(LogLevel.Debug, "Explicit length %s", nChars)

    if (nChars == 0 && isEmptyAllowed) {
      log(LogLevel.Debug, "%s - explicit length of 0 and %ES; found as nilValue.", eName)
      start.thisElement.setNilled()
      return // Empty, no need to advance
    }

    val dis = start.inStream.dataInputStream
    val rawField = dis.getString(nChars).getOrElse {
      PE(start, "%s - %s - Parse failed. Failed to find exactly %s characters.", this.toString(), eName, nChars)
      return
    }
    val fieldLength = rawField.length

    log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValues, nilValues.length)

    val bytePos = (start.bitPos >> 3).toInt
    log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, start.bitPos)
    log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

    // We have a field, is it empty?
    val field = trimByJustification(rawField)
    val isFieldEmpty = field.length() == 0

    if (isFieldEmpty && isEmptyAllowed) {
      // Valid!
      start.thisElement.setNilled()
      return // Empty, no need to advance
    } else if (isFieldEmpty && !isEmptyAllowed) {
      // Fail!
      PE(start, "%s - Empty field found but not allowed!", eName)
      return
    } else if (isFieldNilLiteral(field)) {
      // Contains a nilValue, Success!
      start.thisElement.setNilled()
      log(LogLevel.Debug, "%s - Found %s", eName, rawField)
      return
    } else {
      // Fail!
      PE(start, "%s - Does not contain a nil literal!", eName)
      return
    }
  }
}

class LiteralNilKnownLengthInBytesParser(
  override val parsingPadChar: Maybe[Char],
  val justificationTrim: TextJustificationType.Type,
  lengthInBytes: Long,
  erd: ElementRuntimeData,
  eName: String,
  nilValues: List[String])
  extends LiteralNilInBytesParserBase(erd, eName, nilValues) {

  final def computeLength(start: PState) = {
    lengthInBytes
  }
}

class LiteralNilExplicitLengthInBytesParser(
  override val parsingPadChar: Maybe[Char],
  val justificationTrim: TextJustificationType.Type,
  erd: ElementRuntimeData,
  eName: String,
  expr: CompiledExpression,
  nilValues: List[String])
  extends LiteralNilInBytesParserBase(erd, eName, nilValues) {
  val exprText = expr.prettyExpr

  final def computeLength(start: PState) = {
    val nBytesAsAny = expr.evaluate(start)
    val nBytes = AsIntConverters.asLong(nBytesAsAny) //nBytesAsAny.asInstanceOf[Long]
    nBytes
  }
}

abstract class LiteralNilParserBase(
  erd: ElementRuntimeData,
  eName: String,
  nilValues: List[String])
  extends PrimParser(erd) with PaddingRuntimeMixin {
  val name = erd.prettyName

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + name + " nilValue='" + nilValues + "'/>"
  }

  val isEmptyAllowed = nilValues.contains("%ES;")

}

abstract class LiteralNilInBytesParserBase(
  erd: ElementRuntimeData,
  eName: String,
  override val nilValues: List[String])
  extends LiteralNilParserBase(erd, eName, nilValues)
  with NilMatcherMixin {

  protected def computeLength(start: PState): Long

  def parse(start: PState): Unit = {
    //      withLoggingLevel(LogLevel.Debug) 
    {

      // TODO: What if someone passes in nBytes = 0 for Explicit length, is this legal?

      val nBytes: Long = computeLength(start)

      log(LogLevel.Debug, "Explicit length %s", nBytes)

      log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValues, nilValues.length)
      val in = start.inStream

      val bytePos = (start.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, start.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      // some encodings aren't whole bytes
      // if (start.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

      val decoder = erd.encodingInfo.knownEncodingCharset.charset.newDecoder()

      try {
        val reader = in.getCharReader(erd.encodingInfo.knownEncodingCharset.charset, start.bitPos)
        val bytes = in.getBytes(start.bitPos, nBytes.toInt)
        val cb = decoder.decode(ByteBuffer.wrap(bytes))
        val result = cb.toString
        val trimmedResult = trimByJustification(result)
        val endBitPos = start.bitPos + (nBytes.toInt * 8)
        val endCharPos = if (start.charPos == -1) result.length() else start.charPos + result.length()

        // We have a field, is it empty?
        val isFieldEmpty = trimmedResult.length == 0 //result.length() == 0

        if (isFieldEmpty && isEmptyAllowed) {
          // Valid!
          start.thisElement.setNilled()
          return // Empty, no need to advance
        } else if (isFieldEmpty && !isEmptyAllowed) {
          // Fail!
          PE(start, "%s - Empty field found but not allowed!", eName)
          return
        } else if (isFieldNilLiteral(trimmedResult)) {
          // Contains a nilValue, Success!
          start.thisElement.setNilled()

          log(LogLevel.Debug, "%s - Found %s", eName, trimmedResult)
          log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
          log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

          start.setPos(endBitPos, endCharPos, One(reader)) // Need to advance past found nilValue
          return
        } else {
          // Fail!
          PE(start, "%s - Does not contain a nil literal!", eName)
          return
        }
      } catch {
        case e: IndexOutOfBoundsException => {
          // In this case, we failed to get the bytes
          if (isEmptyAllowed) {
            // Valid!
            start.thisElement.setNilled()
            return // Empty, no need to advance
          } else {
            PE(start, "%s - Insufficient Bytes in field; required %s", name, nBytes)
            return
          }
        }
        case u: UnsuppressableException => throw u
        case e: java.nio.charset.CharacterCodingException => {
          PE(start, "%s - Exception: \n%s", name, e.getMessage())
          return
        }
      }
    }
  }

}