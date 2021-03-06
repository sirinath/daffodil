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

import java.text.ParsePosition
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.{ Calendar, TimeZone, GregorianCalendar, ULocale }
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.CalendarPatternKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.CalendarFirstDayOfWeek
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.CalendarCheckPolicy

case class ConvertTextCalendarParser(gram: Gram, e: ElementBase, dataFormatter: SimpleDateFormat, infosetFormatter: SimpleDateFormat) extends PrimParser(gram, e) {

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1) }

  override def toString = "to(xs:" + GramName + ")"

  def parse(start: PState): PState = {
    val node = start.parentElement
    var str = node.dataValue

    Assert.invariant(str != null)

    val pos = new ParsePosition(0)
    val cal = dataFormatter.getCalendar.clone.asInstanceOf[Calendar]
    dataFormatter.parse(str, cal, pos);

    // Verify that what was parsed was what was passed exactly in byte count
    // Use pos to verify all characters consumed & check for errors
    if (pos.getIndex != str.length || pos.getErrorIndex >= 0) {
      val errIndex = if (pos.getErrorIndex >= 0) pos.getErrorIndex else pos.getIndex
      return PE(start, "Convert to %s (for xs:%s): Failed to parse '%s' at character %d.", GramDescription, GramName, str, errIndex + 1)
    }

    // Unfortunately, there is no publicly available method for validating
    // Calendar values are correct with respect to leniency. So instead, just
    // try to calculate the time, which forces validation. This causes an
    // exception to be thrown if a Calendar is not valid.
    try {
      cal.getTime
    } catch {
      case e: IllegalArgumentException => {
        return PE(start, "Convert to %s (for xs:%s): Failed to parse '%s': %s.", GramDescription, GramName, str, e.getMessage)
      }
    }

    // Convert to the infoset format
    val result = infosetFormatter.format(cal)
    node.setDataValue(result)

    start
  }
}

object TextCalendarConstants {
  final val maxFractionalSeconds = 9
}

abstract class ConvertTextCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends Terminal(e, guard) {

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1) }

  override def toString = "to(xs:" + GramName + ")"

  protected def infosetPattern: String
  protected def implicitPattern: String
  protected def validFormatCharacters: Seq[Char]

  lazy val pattern: String = {
    val p = e.calendarPatternKind match {
      case CalendarPatternKind.Explicit => e.calendarPattern
      case CalendarPatternKind.Implicit => implicitPattern
    }

    val escapedText = "(''|'[^']+'|[^a-zA-Z])".r
    val patternNoEscapes = escapedText.replaceAllIn(p, "")
    patternNoEscapes.toSeq.foreach(char =>
      if (!validFormatCharacters.contains(char)) {
        SDE("Character '%s' not allowed in dfdl:calendarPattern for %s".format(char, GramName))
      })

    if (patternNoEscapes.indexOf("S" * (TextCalendarConstants.maxFractionalSeconds + 1)) >= 0) {
      SDE("More than %d fractional seconds unsupported in dfdl:calendarPattern for %s".format(TextCalendarConstants.maxFractionalSeconds, GramName))
    }

    p
  }

  lazy val locale: ULocale = {
    val canonicalCalLang = ULocale.canonicalize(e.calendarLanguage)
    val l = new ULocale(canonicalCalLang)
    l
  }

  // Used to configure the dataFormatter
  lazy val calendar: Calendar = {
    val cal = Calendar.getInstance(locale)

    val firstDay = e.calendarFirstDayOfWeek match {
      case CalendarFirstDayOfWeek.Sunday => Calendar.SUNDAY
      case CalendarFirstDayOfWeek.Monday => Calendar.MONDAY
      case CalendarFirstDayOfWeek.Tuesday => Calendar.TUESDAY
      case CalendarFirstDayOfWeek.Wednesday => Calendar.WEDNESDAY
      case CalendarFirstDayOfWeek.Thursday => Calendar.THURSDAY
      case CalendarFirstDayOfWeek.Friday => Calendar.FRIDAY
      case CalendarFirstDayOfWeek.Saturday => Calendar.SATURDAY
    }
    cal.setFirstDayOfWeek(firstDay)

    cal.setMinimalDaysInFirstWeek(e.calendarDaysInFirstWeek.toInt)

    val lax = e.calendarCheckPolicy match {
      case CalendarCheckPolicy.Strict => false
      case CalendarCheckPolicy.Lax => true
    }
    cal.setLenient(lax)

    val TimeZoneRegex = """(UTC)?([+\-])?([01]\d|\d)(:?([0-5]\d))?""".r
    val tzStr = e.calendarTimeZone match {
      case TimeZoneRegex(_, plusOrMinus, hour, _, minute) => {
        val pomStr = if (plusOrMinus == null) "+" else plusOrMinus
        val minStr = if (minute == null) "" else minute
        "GMT%s%s%s".format(pomStr, hour, minStr)
      }
      case _ => e.calendarTimeZone
    }

    val tz = TimeZone.getTimeZone(tzStr)
    if (tz == TimeZone.UNKNOWN_ZONE) {
      e.schemaDefinitionErrorDueToPropertyValue(
        "calendarTimeZone", e.calendarTimeZone, e.calendarTimeZone_location,
        "Unknown time zone '%s'", e.calendarTimeZone)
    }
    cal.setTimeZone(tz)

    cal.clear

    cal
  }

  // Used for parsing/unparsing
  lazy val dataFormatter: SimpleDateFormat = {
    val formatter = new SimpleDateFormat(pattern, locale)
    formatter.setCalendar(calendar)
    formatter
  }

  // Used for writing to/reading from the infoset
  lazy val infosetFormatter: SimpleDateFormat = {
    val formatter = new SimpleDateFormat(infosetPattern)
    val cal = new GregorianCalendar()
    cal.clear
    formatter.setCalendar(cal)
    formatter.setTimeZone(TimeZone.GMT_ZONE)
    formatter
  }

  def parser: Parser = new ConvertTextCalendarParser(this, e, dataFormatter, infosetFormatter)

  def unparser: Unparser = DummyUnparser(e)
}

case class ConvertTextDatePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val infosetPattern = "uuuu-MM-ddxxx"
  protected override val implicitPattern = "uuuu-MM-dd"
  protected override val validFormatCharacters = "dDeEFGMuwWyXxYzZ".toSeq
}

case class ConvertTextTimePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val infosetPattern = "HH:mm:ss.SSSSSSxxx"
  protected override val implicitPattern = "HH:mm:ssZ"
  protected override val validFormatCharacters = "ahHkKmsSvVzXxZ".toSeq
}

case class ConvertTextDateTimePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val infosetPattern = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx"
  protected override val implicitPattern = "uuuu-MM-dd'T'HH:mm:ss"
  protected override val validFormatCharacters = "adDeEFGhHkKmMsSuwWvVyXxYzZ".toSeq
}
