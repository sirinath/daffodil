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

import java.util.regex.Matcher
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.lang.Byte
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * Replace character entities, as well as hex/decimal numeric character entities by their unicode codepoint values.
 *
 * Deals with self-escaping of the '%' which introduces a DFDL character entity.
 *
 * Does not deal with raw %#rHH; entities. Those have to be preserved because they get interpreted differently
 * depending on how the string literal is used. Similarly the character class entities like %WSP*; which are used
 * to provide pattern match literals (like delimiters).
 *
 */
class EntityReplacer {

  val dfdlEntityName = "NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC[1-4]|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|NBSP|NEL|LS"
  val dfdlCharClassEntityName = "NL|WSP|WSP\\*|WSP\\+|ES"

  val entityCharacterUnicode: List[(String, String, Pattern)] =
    List(
      ("NUL", "\u0000", Pattern.compile("%" + "NUL" + ";", Pattern.MULTILINE)),
      ("SOH", "\u0001", Pattern.compile("%" + "SOH" + ";", Pattern.MULTILINE)),
      ("STX", "\u0002", Pattern.compile("%" + "STX" + ";", Pattern.MULTILINE)),
      ("ETX", "\u0003", Pattern.compile("%" + "ETX" + ";", Pattern.MULTILINE)),
      ("EOT", "\u0004", Pattern.compile("%" + "EOT" + ";", Pattern.MULTILINE)),
      ("ENQ", "\u0005", Pattern.compile("%" + "ENQ" + ";", Pattern.MULTILINE)),
      ("ACK", "\u0006", Pattern.compile("%" + "ACK" + ";", Pattern.MULTILINE)),
      ("BEL", "\u0007", Pattern.compile("%" + "BEL" + ";", Pattern.MULTILINE)),
      ("BS", "\u0008", Pattern.compile("%" + "BS" + ";", Pattern.MULTILINE)),
      ("HT", "\u0009", Pattern.compile("%" + "HT" + ";", Pattern.MULTILINE)),
      ("LF", "\u000A", Pattern.compile("%" + "LF" + ";", Pattern.MULTILINE)),
      ("VT", "\u000B", Pattern.compile("%" + "VT" + ";", Pattern.MULTILINE)),
      ("FF", "\u000C", Pattern.compile("%" + "FF" + ";", Pattern.MULTILINE)),
      ("CR", "\u000D", Pattern.compile("%" + "CR" + ";", Pattern.MULTILINE)),
      ("SO", "\u000E", Pattern.compile("%" + "SO" + ";", Pattern.MULTILINE)),
      ("SI", "\u000F", Pattern.compile("%" + "SI" + ";", Pattern.MULTILINE)),
      ("DLE", "\u0010", Pattern.compile("%" + "DLE" + ";", Pattern.MULTILINE)),
      ("DC1", "\u0011", Pattern.compile("%" + "DC1" + ";", Pattern.MULTILINE)),
      ("DC2", "\u0012", Pattern.compile("%" + "DC2" + ";", Pattern.MULTILINE)),
      ("DC3", "\u0013", Pattern.compile("%" + "DC3" + ";", Pattern.MULTILINE)),
      ("DC4", "\u0014", Pattern.compile("%" + "DC4" + ";", Pattern.MULTILINE)),
      ("NAK", "\u0015", Pattern.compile("%" + "NAK" + ";", Pattern.MULTILINE)),
      ("SYN", "\u0016", Pattern.compile("%" + "SYN" + ";", Pattern.MULTILINE)),
      ("ETB", "\u0017", Pattern.compile("%" + "ETB" + ";", Pattern.MULTILINE)),
      ("CAN", "\u0018", Pattern.compile("%" + "CAN" + ";", Pattern.MULTILINE)),
      ("EM", "\u0019", Pattern.compile("%" + "EM" + ";", Pattern.MULTILINE)),
      ("SUB", "\u001A", Pattern.compile("%" + "SUB" + ";", Pattern.MULTILINE)),
      ("ESC", "\u001B", Pattern.compile("%" + "ESC" + ";", Pattern.MULTILINE)),
      ("FS", "\u001C", Pattern.compile("%" + "FS" + ";", Pattern.MULTILINE)),
      ("GS", "\u001D", Pattern.compile("%" + "GS" + ";", Pattern.MULTILINE)),
      ("RS", "\u001E", Pattern.compile("%" + "RS" + ";", Pattern.MULTILINE)),
      ("US", "\u001F", Pattern.compile("%" + "US" + ";", Pattern.MULTILINE)),
      ("SP", "\u0020", Pattern.compile("%" + "SP" + ";", Pattern.MULTILINE)),
      ("DEL", "\u007F", Pattern.compile("%" + "DEL" + ";", Pattern.MULTILINE)),
      ("NBSP", "\u00A0", Pattern.compile("%" + "NBSP" + ";", Pattern.MULTILINE)),
      ("NEL", "\u0085", Pattern.compile("%" + "NEL" + ";", Pattern.MULTILINE)),
      ("LS", "\u2028", Pattern.compile("%" + "LS" + ";", Pattern.MULTILINE)))

  val escapeReplacements: List[(String, String, Pattern)] = List(("%", "\u0025", Pattern.compile("%%", Pattern.MULTILINE)))

  val charEntityPattern = Pattern.compile("%(" + dfdlEntityName + ");", Pattern.MULTILINE)
  val hexPattern = Pattern.compile("%#x[0-9a-fA-F]+;", Pattern.MULTILINE)
  val decPattern = Pattern.compile("%#[0-9]+;", Pattern.MULTILINE)
  val bytePattern = Pattern.compile("%#r[0-9a-fA-F]{2};", Pattern.MULTILINE)
  val charClassEntityPattern = Pattern.compile("%(" + dfdlCharClassEntityName + ");", Pattern.MULTILINE)

  val charEntityRegex = ("(%(?:" + dfdlEntityName + ");)(.*)").r
  val hexRegex = "(%#x[0-9a-fA-F]+;)(.*)".r
  val decRegex = "(%#[0-9]+;)(.*)".r
  val byteRegex = "(%#r[0-9a-fA-F]{2};)(.*)".r
  val charClassEntityRegex = ("(%(?:" + dfdlCharClassEntityName + ");)(.*)").r
  val dfdlEntityRegex = "(%[^%]*?;)(.*)".r

  def hasDfdlEntity(input: String): Boolean = {
    if (hasDfdlCharEntity(input) ||
      hasDecimalCodePoint(input) ||
      hasHexCodePoint(input) ||
      hasByteCodePoint(input) ||
      hasDfdlCharClassEntity(input)) {
      return true
    }
    false
  }

  private def isMatched(input: String, p: Pattern): Boolean = p.matcher(input).find()

  def hasDfdlCharClassEntity(input: String): Boolean = isMatched(input, charClassEntityPattern)
  def hasDfdlCharEntity(input: String): Boolean = isMatched(input, charEntityPattern)
  def hasDecimalCodePoint(input: String): Boolean = isMatched(input, decPattern)
  def hasHexCodePoint(input: String): Boolean = isMatched(input, hexPattern)
  def hasByteCodePoint(input: String): Boolean = isMatched(input, bytePattern)

  def replaceHex(input: String, prefix: String): String = {
    var res: String = input

    // While we have Hex Code Points in the string
    // Find and replace with their character equivalents.
    while (hasHexCodePoint(res)) {
      val p: Pattern = hexPattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr, 16)
        val newChar = intStr.toChar.toString
        // Special case here
        // $ is used by replaceAll to refer to prior groups
        // so $ must be escaped into \$
        val newCharNotDollar = if (newChar == "$") """\$""" else newChar
        res = res.replaceAll(rawStr, newCharNotDollar)
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceDecimal(input: String, prefix: String): String = {
    var res: String = input

    // While we have Decimal Code Points in the string
    // Find and replace with their character equivalents.
    while (hasDecimalCodePoint(res)) {
      val p: Pattern = decPattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr, 10)

        res = res.replaceAll(rawStr, intStr.asInstanceOf[Char].toString())
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceBytes(input: String, prefix: String): String = {
    var res: String = input

    // While we have Raw Byte entities in the string
    // Find and replace with their character equivalents.
    while (hasByteCodePoint(res)) {
      val p: Pattern = bytePattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val upperNibble: Int = Byte.parseByte(trimmedStr.substring(0, 1), 16) << 4
        val lowerNibble: Byte = Byte.parseByte(trimmedStr.substring(1, 2), 16)
        val byteStr: Int = upperNibble | lowerNibble //Byte.parseByte(trimmedStr, 16)

        res = res.replaceAll(rawStr, byteStr.toChar.toString)
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceByte(input: String): String = {
    replaceBytes(input, "%#r")
  }

  def replaceHex(input: String): String = {
    replaceHex(input, "%#x")
  }

  def replaceDecimal(input: String): String = {
    replaceDecimal(input, "%#")
  }

  def replaceDfdlEntity(input: String): String = {
    replace(input, entityCharacterUnicode)
  }

  def replaceEscapes(input: String): String = {
    replace(input, escapeReplacements)
  }

  private def replaceEntity(proposedEntity: String, orig: String, context: Option[ThrowsSDE]): String = {
    val result = proposedEntity match {
      case charClassEntityRegex(_, _) => { proposedEntity } // WSP, WSP+/*, NL, etc. Don't get replaced.
      case hexRegex(entity, rest) => { replaceHex(proposedEntity) }
      case decRegex(entity, rest) => { replaceDecimal(proposedEntity) }
      case byteRegex(entity, rest) => { replaceByte(proposedEntity) }
      case charEntityRegex(entity, rest) => { replace(proposedEntity, entityCharacterUnicode) }
      case dfdlEntityRegex(invalidEntity, rest) => {
        // Because we didn't match any of the previously acceptable formats
        // this must be an invalid entity since it's still in the generic
        // %<something>; dfdl entity format.
        context match {
          case Some(ctxt) => ctxt.SDE("Invalid DFDL Entity (%s) found in \"%s\"", invalidEntity, orig)
          case None => {
            val msg = "Invalid DFDL Entity (%s) found in \"%s\"".format(invalidEntity, orig)
            throw new Exception(msg)
          }
        }
      }
      case nonEntity => nonEntity
    }
    result
  }

  /**
   * We'll consider something 'malformed' if:
   * 	1. It starts with a '%' but is not terminated by ';'. Ex: %foo
   * 	2. Within it it has '%' followed by any character (not a '%' or ';') followed by '%'. Ex: %foo%bar;
   *  	3. Within it it has '%#' but is not terminated by ';'. Ex: %#foo
   *  	3. Has a '%' immediately followed by ';'. Ex: %;
   */
  private val malformedEntityFormat = Pattern.compile("((?:%[^%#;]*?%)|(?:%[^%#;]*?$)|(?:%#[^%;]*?$)|(?:%;))", Pattern.MULTILINE)
  private def checkForMalformedEntityFormat(input: String, orig: String, context: Option[ThrowsSDE]) = {
    // At this point, we're assuming the escaped percent literals have already been removed.
    // So we want to look for malformed entities just as a preliminary check.

    val m = malformedEntityFormat.matcher(input)
    if (m.find()) {
      val invalidEntity = m.group(1)
      context match {
        case Some(ctxt) => ctxt.SDE("Invalid DFDL Entity (%s) found in \"%s\"", invalidEntity, orig)
        case None => {
          val msg = "Invalid DFDL Entity (%s) found in \"%s\"".format(invalidEntity, orig)
          throw new Exception(msg)
        }
      }
    }
  }

  private def process(input: String, orig: String, context: Option[ThrowsSDE]): String = {
    if (!input.contains("%")) { return input }

    // Has a % in it, possibly an entity.  Try to see if we can
    // detect if it's malformed.
    checkForMalformedEntityFormat(input, orig, context)

    val tokens = input.split("""(?<!%)%""")
    val tokens2 = tokens.map(tok => (tok, tok.split("[^%]*?;")))

    val tokens3 = tokens2.map {
      case (ent: String, Array()) => {
        // The initial split for 'tokens' removed the %
        // we have to add it back in here.
        replaceEntity("%" + ent, orig, context)
      }
      case (ent: String, Array("", rest)) => {
        // The initial split for 'tokens' removed the %
        // we have to add it back in here.
        replaceEntity("%" + ent, orig, context)
      }
      case (tok: String, Array(tok2)) => {
        Assert.invariant(tok == tok2) // not an entity
        tok
      }
      case _ => Assert.impossibleCase
    }
    val res = tokens3.mkString
    res
  }

  private def hasDoublePercentEnding(input: String): Boolean = {
    if (input == "" || input.length() < 2) false
    else {
      if ((input.charAt(input.length() - 2) == '%') && (input.last == '%')) true
      else false
    }
  }

  /**
   * Replaces all valid dfdl entities with their appropriate values.
   */
  def replaceAll(input: String, context: Option[ThrowsSDE] = None): String = {
    if (!input.contains("%")) { return input } // No entities, no replacement.

    val startOfPossibleEntity = input.indexOf("%")
    val inputUntilPossibleEntity = input.substring(0, startOfPossibleEntity)
    val inputWithPossibleEntity = input.substring(startOfPossibleEntity)

    if (!inputWithPossibleEntity.contains("%%")) {
      // No escaped percents, just process
      val processedInput = process(inputWithPossibleEntity, input, context)
      val fullResult = inputUntilPossibleEntity + processedInput
      return fullResult
    }

    // We have escaped percent literals, we need to also determine if we ended
    // in an escaped percent.  If so, we'll need to append it to the result.
    val endedWithDoublePercent = hasDoublePercentEnding(inputWithPossibleEntity)
    val splitByDoublePercent = inputWithPossibleEntity.split("%%") // Effectively removes escaped percents

    // Below we process each token and at the end call mkString to add back in
    // the escaped % literals if necessary. This works automatically except in the case where a
    // double percent occurred at the end of the input.
    val replaced = splitByDoublePercent.map(token => process(token, input, context))
    val recomposedWithLiteralPercents = inputUntilPossibleEntity + replaced.mkString("%") + (if (endedWithDoublePercent) "%" else "")
    recomposedWithLiteralPercents
  }

  // Replacement helper function
  // 	entity = what you are replacing (informational only)
  // 	unicode = what you are replacing it with
  // 	pattern = what you are replacing via RegEx match on this pattern
  private def replace(input: String, chars: List[(String, String, Pattern)]): String = {
    var res: String = input
    chars.foreach {
      case (entity, unicode, pattern) => {
        val m: Matcher = pattern.matcher(res)
        res = m.replaceAll(unicode)
      }
    }
    res
  }

}

object EntityReplacer extends EntityReplacer

abstract class StringLiteralBase(rawArg: String) {
  val xmlEntityPattern = new Regex("""&(quot|amp|apos|lt|gt);""", "entity")
  val raw: String = {
    val res = xmlEntityPattern.replaceAllIn(rawArg, m => {
      val sb = scala.xml.Utility.unescape(m.group("entity"), new StringBuilder())
      // There really is no possibility for null to come back as we've made
      // sure to only include valid xml entities in the xmlEntityPattern.
      if (sb == null) {
        Assert.impossible("Failed to replace an xml entity (%s) when converting String Literals.".format(m.group("entity")))
      } else { sb.toString() }
    })
    res
  }
  def cooked: String
}

/**
 * String values in the infoset, string results of DFDL's xpath-like expressions are of this kind.
 *
 *  This is the kind of string literal you can use within an expression.
 */
class StringValueAsLiteral(rawArg: String, context: ThrowsSDE)
  extends StringLiteralBase(rawArg) {
  def cooked = EntityReplacer.replaceAll(raw, Some(context))
}

class SingleCharacterLiteral(rawArg: String, context: ThrowsSDE)
  extends StringValueAsLiteral(rawArg, context) {
  context.schemaDefinitionUnless(cooked.length == 1, "Length of string must be exactly 1 character.")
}

class SingleCharacterLiteralES(rawArg: String, context: ThrowsSDE)
  extends StringValueAsLiteral(rawArg, context) {
  context.schemaDefinitionUnless(cooked.length() == 1 || cooked.length() == 0, "Length of string must be exactly 1 character or be empty.")
}

class OneDelimiterLiteral(rawArg: String, context: ThrowsSDE)
  extends StringLiteralBase(rawArg) {
  def cooked = EntityReplacer.replaceAll(raw, Some(context))
  // deal with raw bytes entities
  // deal with character class entities

  /**
   *  return a regex matcher that matches this individual delimiter
   */
  def matcher = {
    Assert.notYetImplemented()
  }

}

class ListOfStringValueAsLiteral(rawArg: String, context: ThrowsSDE) {
  def cooked = {
    val list = rawArg.split("\\s").toList
    val cookedList: ListBuffer[String] = ListBuffer.empty
    list.foreach(x => {
      val l = new StringValueAsLiteral(x, context)
      cookedList += l.cooked
    })
    cookedList.toList
  }
}

class ListOfSingleCharacterLiteral(rawArg: String, context: ThrowsSDE) {
  def cooked = {
    val list = rawArg.split("\\s")
    val cookedList: ListBuffer[String] = ListBuffer.empty
    list.foreach(x => {
      val l = new SingleCharacterLiteral(x, context)
      cookedList += l.cooked
    })
    cookedList.toList
  }
}
