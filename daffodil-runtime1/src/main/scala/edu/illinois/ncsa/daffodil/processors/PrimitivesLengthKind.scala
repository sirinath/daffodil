package edu.illinois.ncsa.daffodil.processors
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.grammar.Terminal
import scala.util.parsing.input.{ Reader }
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }

abstract class StringLength(e: ElementBase)
  extends DelimParserBase(e, true)
  with TextReader
  with Padded
  with WithParseErrorThrowing {

  val charset = e.knownEncodingCharset
  val stringLengthInBitsFnc = e.knownEncodingStringBitLengthFunction
  val codepointWidth = e.knownEncodingWidthInBits

  //lazy val dp = new DFDLDelimParserStatic(e.knownEncodingStringBitLengthFunction)
  lazy val removePaddingParser: Option[dp.Parser[String]] = dp.generateRemovePaddingParser(justificationTrim, padChar)

  def lengthText: String
  def parserName: String

  def getLength(pstate: PState): (Long, PState)
  def parseInput(start: PState, charset: Charset, nBytes: Long): PState

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = String.format("%sParser(%s)", parserName, lengthText)

    def parse(pstate: PState): PState = withParseErrorThrowing(pstate) {

      log(LogLevel.Debug, "Parsing starting at bit position: %s", pstate.bitPos)

      val (nBytes, start) = getLength(pstate)
      log(LogLevel.Debug, "Explicit length %s", nBytes)

      if (start.bitPos % 8 != 0) { return PE(start, "%s - not byte aligned.", parserName) }

      try {
        val postState = parseInput(start, charset, nBytes)
        return postState
      } catch {
        case m: MalformedInputException => { return PE(start, "%s - MalformedInputException: \n%s", parserName, m.getMessage()) }
        case e: IndexOutOfBoundsException => { return PE(start, "%s - Insufficient Bits in field: IndexOutOfBounds: \n%s", parserName, e.getMessage()) }
        case u: UnsuppressableException => throw u
        case e: Exception => { return PE(start, "%s - Exception: \n%s", parserName, e.getStackTraceString) }
      }
      pstate
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = String.format("%sUnparser(%s)", parserName, lengthText)
    //    val encoder = e.knownEncodingEncoder

    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

trait VariableLength { self: Terminal =>
  // Length is an expression
  private lazy val eb = self.context.asInstanceOf[ElementBase]
  val expr = eb.length
  val exprText = expr.prettyExpr

  def getLength(pstate: PState): (Long, PState) = {
    val R(lengthAsAny, newVMap) = expr.evaluate(pstate.parentElement, pstate.variableMap, pstate)
    val length = lengthAsAny.asInstanceOf[Long]
    val start = pstate.withVariables(newVMap)
    (length, start)
  }
}

trait FixedLength { self: Terminal =>
  // Length is a constant
  private lazy val eb = self.context.asInstanceOf[ElementBase]
  //Assert.invariant(eb.knownEncodingWidthInBits != -1)
  //  def getLength(pstate: PState): (Long, PState) = {
  //    (eb.fixedLength, pstate)
  //  }
}

abstract class StringLengthInChars(e: ElementBase, nChars: Long)
  extends StringLength(e)
  with WithParseErrorThrowing {

  lazy val nCharParser = dp.generateInputNCharactersParser(nChars)

  def getLength(pstate: PState): (Long, PState) = {
    (nChars, pstate)
  }

  def parseInput(start: PState, charset: Charset, nChars: Long): PState = start

  override def parser: DaffodilParser = new PrimParser(this, e) {
    String.format("%sParser(%s)", parserName, lengthText)

    def parse(start: PState): PState = withParseErrorThrowing(start) {

      log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos)

      // no longer require alignment (some encodings aren't whole bytes)
      // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInVariableWidthCharacters - not byte aligned.") }

      log(LogLevel.Debug, "Retrieving reader")

      val reader = getReader(charset, start.bitPos, start)

      val result = dp.parseInputNCharacters(nCharParser, reader, removePaddingParser, justificationTrim)

      result match {
        case _: DelimParseFailure =>
          return PE(start, "Parse failed to find exactly %s characters.", nChars)
        case s: DelimParseSuccess => {

          val parsedField = s.field
          val parsedBits = s.numBits
          val endBitPos = start.bitPos + parsedBits

          log(LogLevel.Debug, "Parsed: %s", parsedField)
          log(LogLevel.Debug, "Ended at bit position: %s", endBitPos)

          val endCharPos = if (start.charPos == -1) nChars else start.charPos + nChars
          val currentElement = start.parentElement
          currentElement.setDataValue(parsedField)
          val postState = start.withPos(endBitPos, endCharPos, Some(s.next))
          postState
        }
      }
    }
  }

}

abstract class StringLengthInBytes(e: ElementBase)
  extends StringLength(e) {

  def formatValue(value: String): String = {
    value
  }

  def parseInput(start: PState, charset: Charset, nBytes: Long): PState = {
    val in = start.inStream
    val decoder = charset.newDecoder()

    val reader = getReader(charset, start.bitPos, start)

    // This next block of lines needs to become functionality of the
    // reader so it can be shared, and decoding is all called from one
    // place. 
    val bytes = in.getBytes(start.bitPos, nBytes.toInt)
    val cb = decoder.decode(ByteBuffer.wrap(bytes))
    val result = cb.toString
    val endBitPos = start.bitPos + stringLengthInBitsFnc(result)
    log(LogLevel.Debug, "Parsed: " + result)
    log(LogLevel.Debug, "Ended at bit position " + endBitPos)
    val endCharPos = start.charPos + result.length
    // 
    // Maintain our global count of number of characters.
    // TODO: get rid of global counter for a dataProcessor-saved one. 
    // 
    DFDLCharCounter.incr(result.length)

    val currentElement = start.parentElement
    val trimmedResult = dp.removePadding(removePaddingParser, justificationTrim, result)
    // Assert.invariant(currentElement.getName != "_document_")
    // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
    // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
    currentElement.setDataValue(formatValue(trimmedResult))
    // 
    // if the number of bytes was a multiple of the codepointWidth then 
    // we will have parsed all the bytes, so the endBitPos and endCharPos 
    // are synchronized still. 
    // 
    val postState = {
      // TODO: Shouldn't the 8 * nBytes really be codepointWidth * nBytes?
      if ((endBitPos - start.bitPos) == (8 * nBytes)) {
        start.withPos(endBitPos, endCharPos, Some(reader))
      } else {
        Assert.invariant((endBitPos - start.bitPos) < (8 * nBytes))
        start.withPos(endBitPos, -1, None)
        // -1 means a subsequent primitive will have to construct
        // a new reader at said bitPosition              
      }
    }

    return postState
  }
}

abstract class HexBinaryLengthInBytes(e: ElementBase)
  extends StringLengthInBytes(e) {

  override val charset: Charset = Charset.forName("ISO-8859-1")
  override val stringLengthInBitsFnc = {
    def stringBitLength(str: String) = {
      // variable width encoding, so we have to convert each character 
      // We assume here that it will be a multiple of bytes
      // that is, that variable-width encodings are all some number
      // of bytes.
      str.getBytes(charset).length * 8
    }
    stringBitLength _
  }
  override def formatValue(value: String) = {
    val hexStr = value.map(c => c.toByte.formatted("%02X")).mkString
    hexStr
  }
}

case class HexBinaryFixedLengthInBytes(e: ElementBase, nBytes: Long)
  extends HexBinaryLengthInBytes(e) with FixedLength {

  lazy val parserName = "HexBinaryFixedLengthInBytes"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
}

case class HexBinaryFixedLengthInBits(e: ElementBase, nBits: Long)
  extends HexBinaryLengthInBytes(e) with FixedLength {

  lazy val parserName = "HexBinaryFixedLengthInBits"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    val nBytes = scala.math.ceil(nBits / 8).toLong
    (nBytes, pstate)
  }
}

case class HexBinaryVariableLengthInBytes(e: ElementBase)
  extends HexBinaryLengthInBytes(e) with VariableLength {

  lazy val parserName = "HexBinaryVariableLengthInBytes"
  lazy val lengthText = exprText
}

case class StringFixedLengthInBytesFixedWidthCharacters(e: ElementBase, nBytes: Long)
  extends StringLengthInBytes(e)
  with FixedLength {

  lazy val parserName = "StringFixedLengthInBytesFixedWidthCharacters"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
  // val maxBytes = CompilerTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long)
  extends StringLengthInBytes(e)
  with FixedLength {

  lazy val parserName = "StringFixedLengthInBytesVariableWidthCharacters"
  lazy val lengthText = nBytes.toString()

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }

  // val maxBytes = CompilerTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, numChars: Long)
  extends StringLengthInChars(e, numChars)
  with FixedLength {

  lazy val parserName = "StringFixedLengthInVariableWidthCharacters"
  lazy val lengthText = e.length.constantAsString

  // val maxBytes = CompilerTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringVariableLengthInBytes(e: ElementBase)
  //extends Terminal(e, true)
  extends StringLengthInBytes(e)
  with VariableLength {

  lazy val parserName = "StringVariableLengthInBytes"
  lazy val lengthText = exprText

  // val maxBytes = CompilerTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringVariableLengthInBytesVariableWidthCharacters(e: ElementBase)
  //extends Terminal(e, true)
  extends StringLengthInBytes(e)
  with VariableLength {

  lazy val parserName = "StringVariableLengthInBytesVariableWidthCharacters"
  lazy val lengthText = exprText

  // val maxBytes = CompilerTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringVariableLengthInVariableWidthCharacters(e: ElementBase)
  extends StringLengthInBytes(e)
  with VariableLength {

  lazy val parserName = "StringVariableLengthInVariableWidthCharacters"
  lazy val lengthText = e.length.constantAsString

  // val maxBytes = CompilerTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringPatternMatched(e: ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing with TextReader with Padded {

  val charset = e.knownEncodingCharset

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "StringPatternMatched"
    val pattern = e.lengthPattern

    // The pattern will always be defined
    lazy val dp = new DFDLDelimParserStatic(e.knownEncodingStringBitLengthFunction)
    lazy val patternParser = dp.generateInputPatternedParser(pattern)

    // TODO: Add parameter for changing CharBuffer size

    val eName = e.toString()

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      // withLoggingLevel(LogLevel.Info) 
      {

        log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3))
        log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at bit position: %s", eName, start.bitPos)

        // some encodings aren't whole bytes.
        // if (start.bitPos % 8 != 0) { return PE(start, "StringPatternMatched - not byte aligned.") }

        val bytePos = (start.bitPos >> 3).toInt

        log(LogLevel.Debug, "Retrieving reader")

        val reader = getReader(charset, start.bitPos, start)

        //        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
        //        val result = d.parseInputPatterned(pattern, reader)
        val result = dp.parseInputPatterned(patternParser, reader)

        val postState = result match {
          case _: DelimParseFailure => {
            // TODO: Is this right? A no match constitutes zero length.  So Nil would need to be checked?
            // Because we check for Nil first, this is valid and allowed.
            PE(start, "%s: No match found!", this.toString())
          }
          case s: DelimParseSuccess => {
            val endBitPos = start.bitPos + s.numBits
            log(LogLevel.Debug, "StringPatternMatched - Parsed: %s", s.field)
            log(LogLevel.Debug, "StringPatternMatched - Ended at bit position %s", endBitPos)

            val endCharPos = if (start.charPos == -1) s.field.length() else start.charPos + s.field.length()
            val currentElement = start.parentElement
            currentElement.setDataValue(s.field)
            start.withPos(endBitPos, endCharPos, Some(s.next))
          }

        }
        postState
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

abstract class StringDelimited(e: ElementBase)
  extends DelimParserBase(e, true)
  with TextReader
  with Padded
  with WithParseErrorThrowing {
  //
  val es = e.optionEscapeScheme
  val esObj = EscapeScheme.getEscapeScheme(es, e)
  val tm = e.allTerminatingMarkup
  val cname = toString

  val eName = e.toString()
  val charset = e.knownEncodingCharset
  val elemBase = e

  //val dp = new DFDLDelimParserStatic(e.knownEncodingStringBitLengthFunction)

  // These static delims are used whether we're static or dynamic
  // because even a dynamic can have some static from enclosing scopes.
  //  val staticDelimsRaw = e.allTerminatingMarkup.filter(x => x.isConstant).map { _.constantAsString }
  val staticDelimsRaw = e.allTerminatingMarkup.filter {
    case (delimValue, _, _) => delimValue.isConstant
  }.map {
    case (delimValue, _, _) => delimValue.constantAsString
  }
  val staticDelimsCooked1 = staticDelimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, e).cooked })
  val staticDelimsCooked = staticDelimsCooked1.flatten
  val (staticDelimsParser, staticDelimsRegex) = dp.generateDelimiter(staticDelimsCooked.toSet)
  val combinedStaticDelimsParser = dp.combineLongest(staticDelimsParser)

  //  def parseMethod(hasDelim: Boolean, delimsParser: dp.Parser[String], delimsRegex: Array[String],
  //    reader: Reader[Char]): DelimParseResult

  def parseMethod(hasDelim: Boolean, delimsParser: dp.Parser[String], delimsRegex: Array[String],
    reader: Reader[Char]): DelimParseResult = {
    // TODO: Change DFDLDelimParser calls to get rid of Array.empty[String] since we're only passing a single list the majority of the time.
    if (esObj.escapeSchemeKind == EscapeSchemeKind.Block) {
      val (escapeBlockParser, escapeBlockEndRegex, escapeEscapeRegex) = dp.generateEscapeBlockParsers2(delimsParser,
        esObj.escapeBlockStart, esObj.escapeBlockEnd, esObj.escapeEscapeCharacter, justificationTrim, padChar, true)
      val removeEscapeBlocksRegex = dp.removeEscapesBlocksRegex(escapeEscapeRegex, escapeBlockEndRegex)
      val parseInputParser = dp.generateInputParser2(dp.emptyParser, delimsParser, Array.empty[String], delimsRegex,
        hasDelim, justificationTrim, padChar, true)
      dp.parseInputEscapeBlock(escapeBlockParser, dp.emptyParser, delimsParser, reader,
        justificationTrim, removeEscapeBlocksRegex, parseInputParser)
    } else if (esObj.escapeSchemeKind == EscapeSchemeKind.Character) {
      val delimsRegexCombined = dp.combineDelimitersRegex(Array.empty[String], delimsRegex)
      val escapeCharacterParser = dp.generateInputEscapeCharacterParser2(delimsParser, delimsRegexCombined,
        hasDelim, esObj.escapeCharacter, esObj.escapeEscapeCharacter, justificationTrim, padChar, true)
      val esRegex = dp.convertDFDLLiteralToRegex(esObj.escapeCharacter)
      val esEsRegex = dp.convertDFDLLiteralToRegex(esObj.escapeEscapeCharacter)
      val removeEscapeCharacterRegex = dp.generateRemoveEscapeCharactersSameRegex(esRegex)
      val removeUnescapedEscapesRegex = dp.removeUnescapedEscapesRegex(esEsRegex, esRegex)
      val removeEscapeEscapesThatEscapeRegex = dp.removeEscapeEscapesThatEscapeRegex(esEsRegex, esRegex)
      val removeEscapeRegex = dp.removeEscapeRegex(esRegex)
      dp.parseInputEscapeCharacter(escapeCharacterParser, dp.emptyParser, delimsParser, reader,
        justificationTrim, removeEscapeCharacterRegex, removeUnescapedEscapesRegex,
        removeEscapeEscapesThatEscapeRegex, removeEscapeRegex, esObj.escapeCharacter,
        esObj.escapeEscapeCharacter)
    } else {
      //d.parseInput(Set.empty[String], delimsCooked.toSet, reader, justificationTrim, padChar)
      val parseInputParser = dp.generateInputParser2(dp.emptyParser, delimsParser, Array.empty[String], delimsRegex,
        hasDelim, justificationTrim, padChar, true)
      dp.parseInput(parseInputParser, dp.emptyParser, delimsParser, reader, justificationTrim)
    }

  }

  def getDelims(pstate: PState): (List[String], Array[String], dp.Parser[String], Option[VariableMap])

  /**
   * Called at compile time in static case, at runtime for dynamic case.
   */
  def errorIfDelimsHaveWSPStar(delims: List[String]): Unit = {
    if (delims.filter(x => x == "%WSP*;").length > 0) {
      // We cannot detect this error until expressions have been evaluated!
      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", eName)
      elemBase.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
    }
  }

  def processResult(result: DelimParseResult, state: PState): PState = {
    result match {
      case f: DelimParseFailure =>
        return parser.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      case s: DelimParseSuccess => {
        val field = s.get
        val numBits = s.numBits
        log(LogLevel.Debug, "%s - Parsed: %s Parsed Bytes: %s (bits %s)", eName, field, numBits / 8, numBits)
        val endCharPos = if (state.charPos == -1) s.numCharsRead else state.charPos + s.numCharsRead
        val endBitPos = state.bitPos + numBits
        val currentElement = state.parentElement
        currentElement.setDataValue(field)
        return state.withPos(endBitPos, endCharPos, Some(s.next))
      }
    }
  }

  def parser: DaffodilParser = new PrimParser(this, e) {
    //    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    override def toString = cname + "(" + tm.map { case (delimValue, _, _) => delimValue.prettyExpr } + ")"

    def parse(start: PState): PState = withParseErrorThrowing(start) {

      val (delimsCooked, delimsRegex, delimsParser, vars) = getDelims(start)

      // We must feed variable context out of one evaluation and into the next.
      // So that the resulting variable map has the updated status of all evaluated variables.
      val postEvalState = vars match {
        case Some(v) => start.withVariables(v)
        case None => start
      }

      log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, delimsCooked, delimsCooked.length)

      val bytePos = (postEvalState.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      val reader = getReader(charset, postEvalState.bitPos, postEvalState)
      val hasDelim = delimsCooked.length > 0

      val result = try {
        parseMethod(hasDelim, delimsParser, delimsRegex, reader)
      } catch {
        case mie: MalformedInputException =>
          throw new ParseError(e, Some(postEvalState), "Malformed input, length: %s", mie.getInputLength())
      }
      processResult(result, postEvalState)
    }
  }

  def unparser: Unparser = new Unparser(e) {
    //    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    override def toString = cname + "(" + tm.map { case (delimValue, _, _) => delimValue.prettyExpr } + ")"

    def unparse(start: UState): UState =
      // withLoggingLevel(LogLevel.Info) 
      {
        val data = start.currentElement.getText

        val encoder = charset.newEncoder()
        start.outStream.setEncoder(encoder)
        start.outStream.fillCharBuffer(data)
        log(LogLevel.Debug, "Unparsed: " + start.outStream.getData)
        start
      }
  }
}

trait StaticDelim { self: StringDelimited =>

  // do this at creation time if we're static
  errorIfDelimsHaveWSPStar(staticDelimsCooked)

  def getDelims(pstate: PState): (List[String], Array[String], dp.Parser[String], Option[VariableMap]) = {
    (staticDelimsCooked, staticDelimsRegex, combinedStaticDelimsParser, None)
  }

}

case class StringDelimitedEndOfDataStatic(e: ElementBase)
  extends StringDelimited(e) with StaticDelim

trait DynamicDelim { self: StringDelimited =>

  override def getDelims(pstate: PState): (List[String], Array[String], dp.Parser[String], Option[VariableMap]) = {
    // We must feed variable context out of one evaluation and into the next.
    // So that the resulting variable map has the updated status of all evaluated variables.
    var vars = pstate.variableMap

    val dynamicDelimsRaw = elemBase.allTerminatingMarkup.filter { case (delimValue, _, _) => !delimValue.isConstant }.map {
      case (delimValue, _, _) =>
        {
          val R(res, newVMap) = delimValue.evaluate(pstate.parentElement, vars, pstate)
          vars = newVMap
          res
        }
    }
    val dynamicDelimsCooked1 = dynamicDelimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, elemBase).cooked })
    val dynamicDelimsCooked = dynamicDelimsCooked1.flatten
    val (dynamicDelimsParser, dynamicDelimsRegex) = dp.generateDelimiter(dynamicDelimsCooked.toSet)

    // Combine dynamic and with static delims if they exist
    val delimsParser = dp.combineLongest(dynamicDelimsParser.union(staticDelimsParser))
    val delimsCooked = dynamicDelimsCooked.union(staticDelimsCooked)
    val delimsRegex = dynamicDelimsRegex.union(staticDelimsRegex)
    //
    // moved check here to avoid need for another abstract method to 
    // factor this out.
    errorIfDelimsHaveWSPStar(delimsCooked)

    (delimsCooked, delimsRegex, delimsParser, Some(vars))
  }

}

case class StringDelimitedEndOfDataDynamic(e: ElementBase)
  extends StringDelimited(e) with DynamicDelim

abstract class HexBinaryDelimited(e: ElementBase) extends StringDelimited(e) {
  override val charset: Charset = Charset.forName("ISO-8859-1")
  override def processResult(result: DelimParseResult, state: PState): PState = {
    result match {
      case f: DelimParseFailure =>
        return parser.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      case s: DelimParseSuccess => {
        val field = s.get
        val numBits = s.numBits
        log(LogLevel.Debug, "%s - Parsed: %s Parsed Bytes: %s (bits %s)", eName, field, numBits / 8, numBits)
        val endCharPos = if (state.charPos == -1) s.numCharsRead else state.charPos + s.numCharsRead
        val endBitPos = state.bitPos + numBits
        val currentElement = state.parentElement
        val hexStr = field.map(c => c.toByte.formatted("%02X")).mkString
        currentElement.setDataValue(hexStr)
        return state.withPos(endBitPos, endCharPos, Some(s.next))
      }
    }
  }
}

case class HexBinaryDelimitedEndOfDataStatic(e: ElementBase)
  extends HexBinaryDelimited(e) with StaticDelim

case class HexBinaryDelimitedEndOfDataDynamic(e: ElementBase)
  extends HexBinaryDelimited(e) with DynamicDelim
