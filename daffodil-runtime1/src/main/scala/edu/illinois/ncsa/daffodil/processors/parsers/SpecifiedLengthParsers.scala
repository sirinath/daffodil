package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.util.OnStack
import java.util.regex.Matcher

abstract class SpecifiedLengthParserBase(eParser: DaffodilParser,
  erd: ElementRuntimeData)
  extends DaffodilParser(erd)
  with WithParseErrorThrowing {

  override lazy val childProcessors = Seq(eParser)

  final def parse(pstate: PState, endBitPos: Long) = {
    log(LogLevel.Debug, "Limiting data to %s bits.", endBitPos)
    val savedLimit = pstate.bitLimit0b
    val startPos0b = pstate.bitPos0b
    pstate.setEndBitLimit(endBitPos)
    eParser.parse1(pstate, erd)
    val limitedLength = endBitPos - startPos0b
    val endOfChildrenPos0b = pstate.bitPos0b
    val childrenLength = endOfChildrenPos0b - startPos0b

    log(LogLevel.Debug, "Restoring data limit to %s bits.", pstate.bitLimit0b)

    pstate.setEndBitLimit(savedLimit)
    pstate.status match {
      case Success => {
        // Check that the parsed length is less than or equal to the length of the parent
        //Assert.invariant(postState2.bitPos <= endBitPos)
        this.PECheck(pstate.bitPos <= endBitPos, "The parsed length of the children (%s bits) was greater than that of the parent (%s bits).", childrenLength, limitedLength)
        pstate.setPos(endBitPos, -1, Nope)
      }
      case _ => //ok
    }
  }

}

class SpecifiedLengthPatternParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  patternString: String)
  extends SpecifiedLengthParserBase(eParser, erd) {

  lazy val pattern = patternString.r.pattern // imagine a really big expensive pattern to compile.

  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val dis = start.inStream.dataInputStream
    val mark = dis.mark
    withMatcher { m =>
      val isMatch = dis.lookingAt(m)

      // That matched or it didn't. We don't care. We care that
      // the lookingAt call advanced the bitPos to after the match
      // which means not at all if there was no match.
      val endBitLimit = dis.bitPos0b

      dis.reset(mark)

      parse(start, endBitLimit)
    }
  }
}

class SpecifiedLengthExplicitBitsParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  dcharset: DFDLCharset,
  length: CompiledExpression,
  toBits: Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  // TODO: These SpecifiedLength* classes need some refactorization. This
  // function and getLength are all copied in numerous places 

  def getBitLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * toBits
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {

    val nBits = getBitLength(pState)
    val in = pState.inStream

    val startBitPos = pState.bitPos
    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(pState.bitPos, nBytes)
      val endBitPos = pState.bitPos + nBits
      parse(pState, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = startBitPos + 0
        parse(pState, endBitPos)
      }
      case u: UnsuppressableException => throw u
      case e: Exception => {
        PE(pState, "SpecifiedLengthExplicitBitsParser - Exception: \n%s", e.getStackTraceString)
      }
    }
  }
}

class SpecifiedLengthExplicitBitsFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nBits: Long,
  dcharset: DFDLCharset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      val nBytes = scala.math.ceil(nBits / 8.0).toLong
      val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + nBits
      parse(start, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bits in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        parse(start, endBitPos)
      }
      case u: UnsuppressableException => throw u
      case ex: Exception => { PE(start, "SpecifiedLengthExplicitBitsFixedParser - Exception: \n%s", ex.getStackTraceString) }
    }
  }
}

class SpecifiedLengthExplicitBytesParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  dcharset: DFDLCharset,
  length: CompiledExpression)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def getLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes
  }

  def parse(pState: PState): Unit = withParseErrorThrowing(pState) {
    val limit = pState.bitLimit0b
    val lenAvailable = pState.bitLimit0b - pState.bitPos0b
    val nBytes = getLength(pState)
    val in = pState.inStream
    val bytes = try {
      in.getBytes(pState.bitPos, nBytes)
    } catch {
      case ex: IndexOutOfBoundsException => {
        PE("Insufficient data. Required %s bytes.%s", nBytes,
          {

            if (limit != -1) " Only %s bytes were available.".format(scala.math.ceil(lenAvailable / 8.0).toLong)
            else ""
          })
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        //        val endBitPos = start.bitPos + 0
        //        val postEState = parse(start, endBitPos)
        //        return postEState
      }
    }
    val endBitPos = pState.bitPos + (nBytes * 8)
    parse(pState, endBitPos)
  }
}

class SpecifiedLengthExplicitBytesFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nBytes: Long,
  dcharset: DFDLCharset)
  extends SpecifiedLengthParserBase(eParser, erd) {

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val in = start.inStream

    try {
      // val bytes = in.getBytes(start.bitPos, nBytes)
      val endBitPos = start.bitPos + (nBytes * 8)
      super.parse(start, endBitPos)
    } catch {
      case ex: IndexOutOfBoundsException => {
        // Insufficient bytes in field, but we need to still allow processing
        // to test for Nils
        val endBitPos = start.bitPos + 0
        super.parse(start, endBitPos)
      }
      case u: UnsuppressableException => throw u
    }
  }
}

/**
 * This is used when length is measured in characters, and couldn't be
 * converted to a computation on length in bytes because a character is encoded as a variable number
 * of bytes, e.g., in utf-8 encoding where a character can be 1 to 4 bytes.
 *
 * This base is used for complex types where we need to know how long the "box"
 * is, that all the complex content must fit within, where that box length is
 * measured in characters. In the complex content case we do not need the string that is all the
 * characters, as we're going to recursively descend and parse it into the complex structure.
 *
 * TODO: Idea - this base also ends up being used for nilLiterals (as of this
 * comment being written 2015-06-30), and there, these two passes, one to
 * measure, and then one to parse, really are redundant. Could change the way nilLiterals are
 * parsed to not use this base, and that could boost performance (maybe...) for nilLiteral-intensive formats.
 */
abstract class SpecifiedLengthExplicitCharactersParserBase(
  eParser: DaffodilParser,
  erd: ElementRuntimeData)
  extends SpecifiedLengthParserBase(eParser, erd) {

  private def maybeBitPosAfterNChars(start: PState, nChars: Long): Maybe[Long] = {
    val dis = start.inStream.dataInputStream
    val mark = dis.mark
    val hasNChars = dis.skipChars(nChars)
    val bitLimitAfterNChars = dis.bitPos0b
    dis.reset(mark)
    if (hasNChars) Maybe(bitLimitAfterNChars)
    else Nope
  }

  protected def getLength(s: PState): Long

  final def parse(start: PState): Unit = withParseErrorThrowing(start) {

    val nChars = getLength(start)
    val bitPosAfterNChars = maybeBitPosAfterNChars(start, nChars).getOrElse {
      PE(start, "%s - %s - Parse failed.  Failed to find exactly %s characters.", this.toString(), erd.name, nChars)
      return
    }

    parse(start, bitPosAfterNChars)
  }
}

final class SpecifiedLengthExplicitCharactersFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nChars: Long)
  extends SpecifiedLengthExplicitCharactersParserBase(eParser, erd) {

  override def getLength(s: PState) = nChars

}

final class SpecifiedLengthExplicitCharactersParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthExplicitCharactersParserBase(eParser, erd) {

  def getLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes
  }

}
