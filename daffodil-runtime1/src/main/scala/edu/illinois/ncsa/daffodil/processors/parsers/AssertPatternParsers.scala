package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.charset._
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Debug
import edu.illinois.ncsa.daffodil.util.PreSerialization
import java.util.regex.Matcher
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.util.OnStack

abstract class AssertPatternParserBase(
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  testPattern: String,
  message: String)
  extends PrimParser(rd)
  with TextReader {

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  // private lazy val compiledPattern = ScalaPatternParser.compilePattern(testPattern, rd)

  lazy val pattern = testPattern.r.pattern // imagine a really big expensive pattern to compile.
  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  final def parse(start: PState): Unit = {
    withParseErrorThrowing(start) {
      val bytePos = (start.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, start.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

      //      if (start.bitPos % 8 != 0) {
      //        PE(start, "%s - not byte aligned.", eName)
      //        return
      //      }
      //
      //      log(LogLevel.Debug, "Retrieving reader")
      //
      //      val reader = getReader(rd.encodingInfo.knownEncodingCharset.charset, start.bitPos, start)
      //
      //      val result = ScalaPatternParser.parseInputPatterned(compiledPattern, reader)
      val dis = start.inStream.dataInputStream
      val mark = dis.mark
      withMatcher { m =>
        val isMatch = dis.lookingAt(m)
        afterParse(start, isMatch, m)
      }
      dis.reset(mark)
    }
  }

  protected def afterParse(start: PState, isMatch: Boolean, matcher: Matcher): Unit
}

class AssertPatternParser(
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  testPattern: String,
  message: String)
  extends AssertPatternParserBase(eName, kindString, rd, testPattern, message) {

  def afterParse(start: PState, isMatch: Boolean, matcher: Matcher) {
    if (isMatch) {
      log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
    } else {
      log(LogLevel.Debug, "Assert Pattern fail for testPattern %s", testPattern)
      val diag = new AssertionFailed(rd.schemaFileLocation, start, message + " " + testPattern)
      start.setFailed(diag)
    }
  }
}

class DiscriminatorPatternParser(
  testPattern: String,
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  message: String)
  extends AssertPatternParserBase(eName, kindString, rd, testPattern, message) {

  def afterParse(start: PState, isMatch: Boolean, matcher: Matcher) {
    if (isMatch) {
      // Only want to set the discriminator if it is true
      // we do not want to modify it unless it's true
      start.setDiscriminator(true)
    } else {
      val diag = new AssertionFailed(rd.schemaFileLocation, start, message)
      start.setFailed(diag)
    }
  }
}