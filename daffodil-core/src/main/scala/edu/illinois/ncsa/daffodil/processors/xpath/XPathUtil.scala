package edu.illinois.ncsa.daffodil.processors.xpath

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

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

import javax.xml.xpath._
import javax.xml.xpath.XPathConstants._
import javax.xml.namespace.QName
import org.jdom.Element
import org.jdom.Text
import org.jdom.Parent
import net.sf.saxon.om.NamespaceConstant
import net.sf.saxon.jdom.NodeWrapper
import net.sf.saxon.jdom.DocumentWrapper
import net.sf.saxon.Configuration
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.xpath.XPathEvaluator
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.LogLevel
import scala.xml.NamespaceBinding
import javax.xml.XMLConstants
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import java.util.HashMap
import edu.illinois.ncsa.daffodil.processors.PState
import scala.util.parsing.combinator.RegexParsers
import edu.illinois.ncsa.daffodil.dsom.LocalElementDecl
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom.PrimType
import com.ibm.icu.text.DateFormat
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.GregorianCalendar
import edu.illinois.ncsa.daffodil.processors.UState
import scala.xml.NodeSeq
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import scala.math.BigDecimal

abstract class DFDLFunction(val name: String, val arity: Int) extends XPathFunction {
  val qName = new QName(XMLUtils.DFDL_NAMESPACE, name)
  val ID = (qName, arity)

  DFDLFunctions.functionList +:= this

  def getContext(pstate: PState): ElementBase = {
    // Assumes that a JDOM element was already created
    val currentElement = pstate.parentElement
    val res = currentElement.schemaComponent(pstate)
    res
  }

  def getContext(ustate: UState): ElementBase = {
    Assert.notYetImplemented()
  }

  // TODO: Do we want to use a List here? Direct access takes time to traverse.
  def evaluate(args: java.util.List[_]): Object = {
    if (args.size() < arity)
      throw new XPathExpressionException("Wrong number of arguments to " + name + ". Should be " + arity + ". Args were: " + args)
    val state = DFDLFunctions.currentPState
    val res = state match {
      // None can happen when we're testing if something is a constant.
      case None => throw new XPathExpressionException("State not bound for use by DFDL expression functions.")
      case Some(pstate) => {
        evaluate1(args, pstate)
      }
    }
    res
  }

  protected def evaluate1(args: java.util.List[_], pstate: PState): Object
}

object DFDLFunctions {
  var functionList: List[DFDLFunction] = Nil

  /**
   * This var must be bound to the current state when an expression is evaluated so that
   * the DFDL functions which access the state can work.
   */
  var currentPState: Option[PState] = None
}

/**
 * DEPRECATED
 */
object DFDLPositionFunction extends DFDLFunction("position", 0) {
  def evaluate1(hasNoArgs: java.util.List[_], pstate: PState): Object = {
    val res: java.lang.Long = pstate.arrayPos
    res
  }
}

/**
 * The new name for the dfdl:position function is dfdl:occursIndex.
 */
object DFDLOccursIndexFunction extends DFDLFunction("occursIndex", 0) {
  def evaluate1(hasNoArgs: java.util.List[_], pstate: PState): Object = {
    val res: java.lang.Long = pstate.arrayPos
    res
  }
}

/**
 * This function converts a string that contains DFDL entites.
 */
object DFDLStringFunction extends DFDLFunction("string", 1) {
  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    val xmlString = args.get(0) match {
      case s: String => s
      case other => {
        // argument wasn't a string
        // 
        // TODO: anyway to inform XPath compiler that this function needs a 
        // string as its argument type? Otherwise we have to do this runtime type check.
        // 
        //getContext(pstate).schemaDefinitionError("Type error. Argument was not a string: %s", other)
        pstate.SDE("Type error. Argument was not a string: %s", other)
      }
    }
    val dfdlString = EntityReplacer.replaceAll(xmlString, Some(pstate))
    val remappedString = XMLUtils.remapXMLIllegalCharactersToPUA(dfdlString)
    remappedString
  }
}

object DFDLContentLengthFunction extends DFDLFunction("contentLength", 2) {
  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    //pstate.failed("dfdl:contentLength is not valid during parsing.")
    //getContext(pstate).schemaDefinitionError("dfdl:contentLength is not valid during parsing.")
    pstate.SDE("dfdl:contentLength is not valid during parsing.")
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    getContext(ustate).notYetImplemented("dfdl:contentLength for unparsing")
  }
}

object DFDLValueLengthFunction extends DFDLFunction("valueLength", 2) {
  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    //pstate.failed("dfdl:valueLength is not valid during parsing.")
    //getContext(pstate).schemaDefinitionError("dfdl:valueLength is not valid during parsing.")
    pstate.SDE("dfdl:valueLength is not valid during parsing.")
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    getContext(ustate).notYetImplemented("dfdl:valueLength for unparsing")
  }
}

object DFDLTestBitFunction extends DFDLFunction("testBit", 2) {

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    val (data, bitPos) = processArgs(args, pstate)
    testBit(data, bitPos)
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    getContext(ustate).notYetImplemented("dfdl:testBit for unparsing")
  }

  def processArgs(args: java.util.List[_], pstate: PState): (Int, Int) = {
    val data = args.get(0)
    val bitPos = args.get(1)

    val dataType = if (data != null) data.asInstanceOf[AnyRef].getClass().getSimpleName() else "null"

    def checkTextInValueSpaceOfUByte(text: String): Int = {
      val i = try { Integer.parseInt(text) } catch {
        case e: Exception => pstate.SDE("dfdl:testBit unable to parse (" + text +
          ") to unsignedByte. $data must be an unsignedByte within the range of (0-255).")
      }
      if (i > 255 || i < 0) { pstate.SDE("dfdl:testBit $data must be an unsignedByte within the range of (0-255). $data was " + i) }
      i
    }

    // According to spec, data should be a byte-value
    //
    // Update: Should only be unsignedByte but it does not
    // appear that we keep type information around in the JDOM tree
    // so we have to do the checking here.
    val dataInt = data match {
      //      case i: Int => {
      //        if (i > 255 || i < 0) { pstate.SDE("dfdl:testBit $data must be an Integer that exists within the value-space of byte.") }
      //        i
      //      }
      //      case bi: java.math.BigInteger => {
      //        val lessThanZero = bi.compareTo(java.math.BigInteger.ZERO) < 0
      //        val greaterThan255 = bi.compareTo(new java.math.BigInteger("255")) > 0
      //        if (lessThanZero || greaterThan255) pstate.SDE("dfdl:testBit requires $data value to be in the value-space of Byte (0-255)")
      //        bi.intValue()
      //      }
      case b: Byte => b.toInt
      case e: Element => checkTextInValueSpaceOfUByte(e.getText())
      case t: Text => checkTextInValueSpaceOfUByte(t.getText())
      case _ => pstate.SDE("dfdl:testBit requires $data to be an unsignedByte. $data evaluated to a(n) " + dataType)

    }
    val bitPosInt = bitPos match {
      case i: Int => {
        // Assumes bitPos 0-7
        if (i < 0 || i > 7) { pstate.SDE("dfdl:testBit requires $bitPos to be an Integer value 0-7.") }
        i
      }
      case bi: java.math.BigInteger => {
        val lessThanZero = bi.compareTo(java.math.BigInteger.ZERO) < 0
        val greaterThan7 = bi.compareTo(new java.math.BigInteger("7")) > 0
        if (lessThanZero || greaterThan7) pstate.SDE("dfdl:testBit requires $bitPos to be an Integer value 0-7.")
        bi.intValue()
      }
      case _ => pstate.SDE("dfdl:testBit requires $bitPos to be an Integer value 0-7")
    }
    (dataInt, bitPosInt)
  }

  def testBit(data: Int, bitPos: Int): java.lang.Boolean = {
    // Assume 8-bit
    val shifted = data >>> bitPos
    val maskedVal = shifted & 1
    if (maskedVal == 1) java.lang.Boolean.TRUE
    else java.lang.Boolean.FALSE
  }
}

object DFDLSetBitsFunction extends DFDLFunction("setBits", 8) {

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    val context = getContext(pstate)
    setBits(args, pstate)
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    getContext(ustate).notYetImplemented("dfdl:setBits for unparsing")
  }
  def processValue(value: Any, pstate: PState): Boolean = {
    value match {
      case i: Integer => {
        if (i < 0 || i > 1) pstate.SDE("dfdl:setBits $bitX must be 0 or 1")
        if (i == 0) false
        else true
      }
      case bi: java.math.BigInteger => {
        val lessThanZero = bi.compareTo(java.math.BigInteger.ZERO) < 0
        val greaterThanOne = bi.compareTo(new java.math.BigInteger("1")) > 0
        if (lessThanZero || greaterThanOne) pstate.SDE("dfdl:setBits requires $bitX to be an Integer value 0 or 1.")
        val i = bi.intValue()
        if (i == 0) false
        else true
      }
      case e: Element => {
        val v = e.getText
        val i = try { Integer.parseInt(v) } catch {
          case e: Exception => pstate.SDE("dfdl:setBits unable to parse (%s) to Integer.", v)
        }
        if (i > 1 || i < 0) { pstate.SDE("dfdl:setBits requires $bitX to be an Integer value 0 or 1.") }
        if (i == 0) false
        else true
      }
      case t: Text => {
        val v = t.getText()
        val i = try { Integer.parseInt(v) } catch {
          case e: Exception => pstate.SDE("dfdl:setBits unable to parse (%s) to Integer.", v)
        }
        if (i > 1 || i < 0) { pstate.SDE("dfdl:setBits requires $bitX to be an Integer value 0 or 1.") }
        if (i == 0) false
        else true
      }
      case _ => pstate.SDE("dfdl:setBits $bitX must be an Integer of a value 0 or 1")
    }
  }
  def setBits(args: java.util.List[_], pstate: PState): java.lang.Integer = {
    val bp0 = processValue(args.get(0), pstate)
    val bp1 = processValue(args.get(1), pstate)
    val bp2 = processValue(args.get(2), pstate)
    val bp3 = processValue(args.get(3), pstate)
    val bp4 = processValue(args.get(4), pstate)
    val bp5 = processValue(args.get(5), pstate)
    val bp6 = processValue(args.get(6), pstate)
    val bp7 = processValue(args.get(7), pstate)
    var uByte: java.lang.Integer = 0
    if (bp0) uByte += 1
    if (bp1) uByte += 2
    if (bp2) uByte += 4
    if (bp3) uByte += 8
    if (bp4) uByte += 16
    if (bp5) uByte += 32
    if (bp6) uByte += 64
    if (bp7) uByte += 128
    uByte
  }
}

object DFDLOccursCountWithDefaultFunction extends DFDLFunction("occursCountWithDefault", 1) {

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    val context = getContext(pstate)
    getContext(pstate).notYetImplemented("dfdl:occursCountWithDefault, defaults aren't implemented")
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    getContext(ustate).notYetImplemented("dfdl:occursCountWithDefault for unparsing, defaults aren't implemented")
  }
}

object DFDLOccursCountFunction extends DFDLFunction("occursCount", 1) {

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    //val context = getContext(pstate)
    val x = args.get(0)
    val occursCount = args.get(0) match {
      case ns: NodeSeq => ns.length
      case se: net.sf.saxon.value.SequenceExtent => {
        se.getLength()
      }
      case _ => pstate.SDE("dfdl:occursCount did not receive a NodeSeq back, check your path.")
    }
    java.lang.Long.valueOf(occursCount)
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    // TODO: context unable to be retrieved for ustate
    //    val context = getContext(ustate)
    //    val x = args.get(0)
    //    val occursCount = args.get(0) match {
    //      case ns: NodeSeq => ns.length
    //      case se: net.sf.saxon.value.SequenceExtent => {
    //        se.getLength()
    //      }
    //      case _ => context.SDE("dfdl:occursCount did not receive a NodeSeq back, check your path.")
    //    }
    //    java.lang.Long.valueOf(occursCount)
    getContext(ustate).notYetImplemented("dfdl:occursCount for unparsing")
  }
}

object DFDLCheckConstraintsFunction extends DFDLFunction("checkConstraints", 1) {
  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._
  import util.control.Breaks._
  import edu.illinois.ncsa.daffodil.dsom.PrimType._

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    // Assumes that a JDOM element was already created
    val expr = args.get(0)
    val currentElement = pstate.parentElement
    val e = pstate.getContext() //getContext(pstate)
    val data = currentElement.dataValue
    val primType = e.primType

    // TODO: Not SimpleType, issue an SDE
    if (!e.isSimpleType) pstate.SDE("dfdl:checkConstraints may only be called on simple types.")

    // We have an ElementBase, retrieve the constraints
    if (e.hasPattern) {
      val patterns = e.patternValues
      if (!currentElement.isNil && patterns.size > 0) {
        val check = checkPatterns(data, patterns)
        if (!check) {
          return java.lang.Boolean.FALSE
        }
      }
    }

    if (e.hasEnumeration) {
      val enumerations = e.enumerationValues
      if (!currentElement.isNil && enumerations.size > 0) {
        val check = checkEnumerations(data, enumerations)
        if (!check) {
          return java.lang.Boolean.FALSE
        }
      }
    }

    // Check minLength
    if (e.hasMinLength) {
      val minLength = e.minLength
      val isMinLengthGreaterThanEqToZero = minLength.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNil && isMinLengthGreaterThanEqToZero) {
        if (!checkMinLength(data, minLength, e, primType)) return java.lang.Boolean.FALSE
      }
    }
    // Check maxLength
    if (e.hasMaxLength) {
      val maxLength = e.maxLength
      val isMaxLengthGreaterThanEqToZero = maxLength.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNil && isMaxLengthGreaterThanEqToZero) {
        if (!checkMaxLength(data, maxLength, e, primType)) return java.lang.Boolean.FALSE
      }
    }
    // Check minInclusive
    if (e.hasMinInclusive) {
      val minInclusive = e.minInclusive
      if (!currentElement.isNil) {
        if (!checkMinInc(data, minInclusive, primType, e)) return java.lang.Boolean.FALSE
      }
    }
    // Check maxInclusive
    if (e.hasMaxInclusive) {
      val maxInclusive = e.maxInclusive
      if (!currentElement.isNil) {
        if (!checkMaxInc(data, maxInclusive, primType, e)) return java.lang.Boolean.FALSE
      }
    }
    // Check minExclusive
    if (e.hasMinExclusive) {
      val minExclusive = e.minExclusive
      if (!currentElement.isNil) {
        if (!checkMinExc(data, minExclusive, primType, e)) return java.lang.Boolean.FALSE
      }
    }
    // Check maxExclusive
    if (e.hasMaxExclusive) {
      val maxExclusive = e.maxExclusive
      if (!currentElement.isNil) {
        if (!checkMaxExc(data, maxExclusive, primType, e)) return java.lang.Boolean.FALSE
      }
    }
    // Check totalDigits
    if (e.hasTotalDigits) {
      val totalDigits = e.totalDigits
      val isTotalDigitsGreaterThanEqToZero = totalDigits.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNil && isTotalDigitsGreaterThanEqToZero) {
        if (!checkTotalDigits(data, totalDigits)) return java.lang.Boolean.FALSE
      }
    }
    // Check fractionDigits
    if (e.hasFractionDigits) {
      val fractionDigits = e.fractionDigits
      val isFractionDigitsGreaterThanEqToZero = fractionDigits.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNil && isFractionDigitsGreaterThanEqToZero) {
        if (!checkFractionDigits(data, fractionDigits)) return java.lang.Boolean.FALSE
      }
    }

    // Note: dont check occurs counts // if(!checkMinMaxOccurs(e, pstate.arrayPos)) { return java.lang.Boolean.FALSE }
    java.lang.Boolean.TRUE
  }

  def checkMinLength(data: String, minValue: java.math.BigDecimal,
    e: ElementBase, primType: PrimType.Type): java.lang.Boolean = {
    primType match {
      case PrimType.String => {
        val bdData = new java.math.BigDecimal(data.length())
        val isDataLengthLess = bdData.compareTo(minValue) < 0
        if (isDataLengthLess) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        // Has to come through as a string in infoset
        // hex string is exactly twice as long as number of bytes
        // take length / 2 = length
        val bdDataLength = new java.math.BigDecimal(data.length() / 2)
        val isDataLengthEqual = bdDataLength.compareTo(minValue) == 0
        if (isDataLengthEqual) java.lang.Boolean.TRUE
        else java.lang.Boolean.FALSE
      }
      case _ => e.SDE("MinLength facet is only valid for string and hexBinary.")
    }
  }

  def checkMaxLength(data: String, maxValue: java.math.BigDecimal,
    e: ElementBase, primType: PrimType.Type): java.lang.Boolean = {
    primType match {
      case PrimType.String => {
        val bdData = new java.math.BigDecimal(data.length())
        val isDataLengthGreater = bdData.compareTo(maxValue) > 0
        if (isDataLengthGreater) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        // Has to come through as a string in infoset
        // hex string is exactly twice as long as number of bytes
        // take length / 2 = length
        val bdDataLength = new java.math.BigDecimal(data.length() / 2)
        val isDataLengthEqual = bdDataLength.compareTo(maxValue) == 0
        if (isDataLengthEqual) java.lang.Boolean.TRUE
        else java.lang.Boolean.FALSE
      }
      case _ => e.SDE("MaxLength facet is only valid for string and hexBinary.")
    }

  }

  // TODO: Duplication of dateToBigDecimal in Types.scala, throw in a library?
  def dateToBigDecimal(date: String, format: String, eb: ElementBase): java.math.BigDecimal = {
    val df = new SimpleDateFormat(format)
    df.setCalendar(new GregorianCalendar())
    df.setTimeZone(TimeZone.GMT_ZONE)
    val dt = try {
      df.parse(date)
    } catch {
      case e: Exception => eb.SDE("Failed to parse date (%s) to format (%s)", date, format)
    }
    new java.math.BigDecimal(dt.getTime())
  }

  // TODO: Duplication of convertFacetToBigDecimal in Types.scala , throw in a library?
  def convertDataToBigDecimal(data: String, primType: PrimType.Type, e: ElementBase): java.math.BigDecimal = {
    primType match {
      case PrimType.DateTime => dateToBigDecimal(data, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx", e)
      case PrimType.Date => dateToBigDecimal(data, "uuuu-MM-ddxxx", e)
      case PrimType.Time => dateToBigDecimal(data, "HH:mm:ss.SSSSSSxxx", e)
      case _ => new java.math.BigDecimal(data)
    }
  }

  def checkMinInc(data: String, minValue: java.math.BigDecimal, primType: PrimType.Type, e: ElementBase): Boolean = {
    //    val bdData = new java.math.BigDecimal(data)
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataGreaterThanEqToMinInc = bdData.compareTo(minValue) >= 0
    isDataGreaterThanEqToMinInc
  }

  def checkMinExc(data: String, minValue: java.math.BigDecimal, primType: PrimType.Type, e: ElementBase): Boolean = {
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataGreaterThanEqToMinExc = bdData.compareTo(minValue) > 0
    isDataGreaterThanEqToMinExc
  }

  def checkMaxInc(data: String, maxValue: java.math.BigDecimal, primType: PrimType.Type, e: ElementBase): Boolean = {
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataLessThanEqToMaxInc = bdData.compareTo(maxValue) <= 0
    isDataLessThanEqToMaxInc
  }

  def checkMaxExc(data: String, maxValue: java.math.BigDecimal, primType: PrimType.Type, e: ElementBase): Boolean = {
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataLessThanMaxExc = bdData.compareTo(maxValue) < 0
    isDataLessThanMaxExc
  }

  def checkTotalDigits(data: String, digits: java.math.BigDecimal): Boolean = {
    // Per http://www.w3.org/TR/xmlschema-2/#rf-totalDigits
    // |i| < 10^totalDigits
    val number = new java.math.BigDecimal(scala.math.pow(10.0, digits.doubleValue()))
    val biNumber = new java.math.BigInteger(number.intValueExact().toString())
    val bdData = new java.math.BigDecimal(data).unscaledValue()
    val isDataLessThanNumber = bdData.compareTo(biNumber) < 0
    isDataLessThanNumber
  }

  def checkFractionDigits(data: String, digits: java.math.BigDecimal): Boolean = {
    val bdData = new java.math.BigDecimal(data)
    // Rounding HALF_DOWN prevents us from accidentally increasing the value.
    val rounded = bdData.setScale(digits.intValue(), java.math.RoundingMode.HALF_DOWN)
    val isDataSameAsRounded = bdData.compareTo(rounded) == 0
    isDataSameAsRounded
  }

  def checkEnumerations(data: String, enumerations: String): Boolean = {
    data.matches(enumerations)
  }

  def checkPatterns(data: String, patterns: Seq[FacetValueR]): Boolean = {
    var isSuccess: Boolean = true

    breakable {
      for (simpleType <- patterns) {
        // each pattern within simpleType is OR'd
        // each pattern between simpleType's is AND'd

        // Each elem represents a simpleType
        // each simpleType is allowed a facetPattern
        // each facetPattern represents all patterns on this particular
        // simpleType.
        //
        // Ex.
        // <SimpleType name="A">
        //   <restriction base="B">
        //     <pattern value="1"/>
        //     <pattern value="2"/>
        //   </restriction>
        // </SimpleType>
        // 
        // <SimpleType name="B">
        //   <restriction base="int">
        //     <pattern value="3"/>
        //     <pattern value="4"/>
        //   </restriction>
        // </SimpleType>
        //
        // Here facetPattern for SimpleType-A = "1|2" (OR'd)
        val (facetName, facetPattern) = simpleType

        // All patterns between simpleTypes must match (AND'd)
        if (!data.matches(facetPattern.toString())) {
          isSuccess = false
          break
        }
      }
    }
    return isSuccess
  }

  def checkMinMaxOccurs(element: ElementBase, position: Long): Boolean = {
    // We only want to fail here if the element is in an array AND
    // the position isn't within the confines of min/max occurs
    //
    if (element.isInstanceOf[LocalElementDecl]) {
      val led = element.asInstanceOf[LocalElementDecl]
      if (!led.isScalar) { return checkOccurrance(led.minOccurs, led.maxOccurs, position) }
    } else if (element.isInstanceOf[GlobalElementDecl]) {
      val ged = element.asInstanceOf[GlobalElementDecl]
      ged.elementRef match {
        case Some(ref) => {
          if (!ref.isScalar) { return checkOccurrance(ref.minOccurs, ref.maxOccurs, position) }
        }
        case None =>
      }
    }
    return true
  }

  def checkOccurrance(minOccurs: Int, maxOccurs: Int, position: Long): Boolean = {
    //System.err.println("checkOccurrance(%s, %s, %s)".format(minOccurs, maxOccurs, position))
    // A maxOccurs of -1 signifies unbounded
    if ( // position > minOccurs && // DON"T CHECK MIN OCCURS. 
    // That can't work. If minOccurs is 5 the first element at position 1 will fail this check.
    ((position <= maxOccurs) || (maxOccurs == -1))) { return true }
    return false
  }
}

object DFDLStringLiteralFromStringFunction extends DFDLFunction("stringLiteralFromString", 1) {

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    //val context = getContext(pstate)
    val x = args.get(0)
    val res = x match {
      case s: String => constructLiteral(s)
      case t: Text => constructLiteral(t.getText())
      case _ => pstate.SDE("dfdl:stringLiteralFromString %s was not a String.", x)
    }
    res
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    getContext(ustate).notYetImplemented("dfdl:stringLiteralFromString for unparsing")
  }

  def constructLiteral(s: String) = {
    val sb = new StringBuilder
    s.foreach(c => {
      c match {
        case '%' => sb.append("%%") // \u0025
        case '\u0000' | 0xE000 => sb.append("%NUL;")
        case '\u0001' | 0xE001 => sb.append("%SOH;")
        case '\u0002' | 0xE002 => sb.append("%STX;")
        case '\u0003' | 0xE003 => sb.append("%ETX;")
        case '\u0004' | 0xE004 => sb.append("%EOT;")
        case '\u0005' | 0xE005 => sb.append("%ENQ;")
        case '\u0006' | 0xE006 => sb.append("%ACK;")
        case '\u0007' | 0xE007 => sb.append("%BEL;")
        case '\u0008' | 0xE008 => sb.append("%BS;")
        case '\u0009' => sb.append("%HT;") // OK, not remapped
        case '\u000A' => sb.append("%LF;") // OK, not remapped
        case '\u000B' | 0xE00B => sb.append("%VT;")
        case '\u000C' | 0xE00C => sb.append("%FF;")
        case '\u000D' => sb.append("%CR;") // OK, not remapped
        case '\u000E' | 0xE00E => sb.append("%SO;")
        case '\u000F' | 0xE00F => sb.append("%SI;")
        case '\u0010' | 0xE010 => sb.append("%DLE;")
        case '\u0011' | 0xE011 => sb.append("%DC1;")
        case '\u0012' | 0xE012 => sb.append("%DC2;")
        case '\u0013' | 0xE013 => sb.append("%DC3;")
        case '\u0014' | 0xE014 => sb.append("%DC4;")
        case '\u0015' | 0xE015 => sb.append("%NAK;")
        case '\u0016' | 0xE016 => sb.append("%SYN;")
        case '\u0017' | 0xE017 => sb.append("%ETB;")
        case '\u0018' | 0xE018 => sb.append("%CAN;")
        case '\u0019' | 0xE019 => sb.append("%EM;") // and above remapped to c + 0xE000
        case '\u001A' => sb.append("%SUB;")
        case '\u001B' => sb.append("%ESC;")
        case '\u001C' => sb.append("%FS;")
        case '\u001D' => sb.append("%GS;")
        case '\u001E' => sb.append("%RS;")
        case '\u001F' => sb.append("%US;")
        case '\u0020' => sb.append("%SP;")
        case '\u007F' => sb.append("%DEL;")
        case '\u00A0' => sb.append("%NBSP;")
        case '\u0085' => sb.append("%NEL;")
        case '\u2028' => sb.append("%LS;")
        case _ => sb.append(c)
      }
    })
    sb.toString()
  }
}

object DFDLContainsEntityFunction extends DFDLFunction("containsEntity", 1) {

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    //val context = getContext(pstate)
    val x = args.get(0)
    val res = x match {
      case s: String => containsEntity(s)
      case t: Text => containsEntity(t.getText())
      case _ => pstate.SDE("dfdl:containsEntity%s was not a String.", x)
    }
    res
  }
  def evaluate1(args: java.util.List[_], ustate: UState): Object = {
    getContext(ustate).notYetImplemented("dfdl:containsEntity for unparsing")
  }

  def containsEntity(s: String): java.lang.Boolean = {
    val e = new EntityReplacer
    e.hasDfdlEntity(s)
  }
}

/**
 * XPath function library for non-built-in functions
 */
object Functions {
  def get(pair: (QName, Int)) = funcs.get(pair)

  val funcList = List(
    DFDLPositionFunction,
    DFDLOccursIndexFunction,
    DFDLStringFunction,
    DFDLCheckConstraintsFunction,
    DFDLOccursCountFunction,
    DFDLOccursCountWithDefaultFunction,
    DFDLSetBitsFunction,
    DFDLTestBitFunction,
    DFDLValueLengthFunction,
    DFDLContentLengthFunction,
    DFDLStringLiteralFromStringFunction,
    DFDLContainsEntityFunction)
  val funcs = funcList.map { f => ((f.ID, f)) }.toMap

}

/**
 * Utility object for evaluating XPath expressions
 */
object XPathUtil extends Logging {

  System.setProperty("javax.xml.xpath.XPathFactory:" + NamespaceConstant.OBJECT_MODEL_JDOM, "net.sf.saxon.xpath.XPathFactoryImpl")
  private val xpathFactory = XPathFactory.newInstance(NamespaceConstant.OBJECT_MODEL_JDOM)

  /**
   * Compile an xpath. It insures functions called actually exist etc.
   * Should help with performance also.
   *
   * Returns a VariableMap=>XPathExpression, that is,
   * a CompiledExpressionFactory
   */
  def compileExpression(dfdlExpressionRaw: String,
    namespaces: Seq[org.jdom.Namespace],
    context: SchemaComponent) =
    // withLoggingLevel(LogLevel.Info) 
    {
      log(LogLevel.Debug, "Compiling expression")
      val dfdlExpression = dfdlExpressionRaw.trim
      Assert.usage(dfdlExpression != "")
      // strip leading and trailing {...} if they are there.
      val expression = if (isExpression(dfdlExpression)) getExpression(dfdlExpression) else dfdlExpression

      // Hack around bug in Saxon JAXP support by casting to Saxon-specific class.
      // -JWC, 27Jul2012.
      // Is it really a bug, or just lack of a standard API?
      // -MikeB 03May2013
      val xpath = xpathFactory.newXPath().asInstanceOf[XPathEvaluator]
      var variables: VariableMap = new VariableMap() // Closed over. This is modified to supply different variables
      log(LogLevel.Debug, "Namespaces: %s", namespaces)

      val nsContext = new javax.xml.namespace.NamespaceContext {

        def getNamespaceURI(prefix: String) = {
          if (prefix == null)
            throw new IllegalArgumentException("The prefix cannot be null.");
          val lookup = namespaces.find { ns => ns.getPrefix == prefix }
          lookup match {
            case None => null
            case Some(ns) => ns.getURI
          }
        }
        def getPrefixes(uri: String) = Assert.invariantFailed("supposed to be unused.")
        def getPrefix(uri: String): String = Assert.invariantFailed("supposed to be unused.")
        //      {
        //        getPrefixList(uri).head
        //      }
        //      private def getPrefixList(uri : String) : Seq[String] = {
        //        val submap = ht.filter{ case (pre, ns) => uri == ns}
        //        val prefixes = submap.map{ case (pre, ns) => pre }
        //        prefixes.toSeq
        //      }
      }

      xpath setNamespaceContext (nsContext)

      //
      // Finish the hack by setting the default element namespace (Saxon's API) 
      // to the default namespace returned by the NamespaceContext (JAXP API).
      // -JWC, 27Jul2012.
      val nsForNoPrefix = nsContext.getNamespaceURI(XMLConstants.DEFAULT_NS_PREFIX)
      val defaultElementNS =
        if (nsForNoPrefix != null) nsForNoPrefix
        else XMLConstants.NULL_NS_URI // Null NS aka No Namespace.
      xpath.getStaticContext().setDefaultElementNamespace(defaultElementNS)

      xpath.setXPathVariableResolver(
        new XPathVariableResolver() {
          def resolveVariable(qName: QName): Object = {
            // FIXME: PERFORMANCE: readVariable should use the QName object, not require this string to be created every time
            // we read a variable.
            val varName = XMLUtils.expandedQName(qName)
            val (res, newVMap) = variables.readVariable(varName, context)
            variables = newVMap
            res
          }
        })

      xpath.setXPathFunctionResolver(
        new XPathFunctionResolver() {
          def resolveFunction(functionName: QName, arity: Int): XPathFunction = {
            val maybeF = Functions.get((functionName, arity))
            maybeF match {
              case None => throw new XPathExpressionException("no such function: " + functionName + " with arity " + arity)
              case Some(f) =>
                f
            }
          }
        })

      val xpathExpr = try {
        xpath.compile(expression)
      } catch {
        case e: XPathExpressionException => {
          val exc = e // debugger never seems to show the case variable itself.
          val realExc = e.getCause()
          val forMsg = if (realExc != null) realExc else exc
          // compilation threw an error. That's a compilation time error, aka a schema definition error
          context.SDE("Expression compiler reports: %s", forMsg)
        }
      }

      // We need to supply the variables late
      val withoutVariables = new CompiledExpressionFactory(expression) {
        def getXPathExpr(runtimeVars: VariableMap) = {
          variables = runtimeVars
          xpathExpr
        }
        // we need to get the variables back at the end of exprsesion evaluation.
        def getVariables() = variables
      }

      withoutVariables // return this factory
    }

  abstract class CompiledExpressionFactory(val expression: String) {
    def getXPathExpr(runtimeVars: VariableMap): XPathExpression
    def getVariables(): VariableMap
  }

  /**
   * For unit testing only.
   * Evaluates an XPath 2 expression in one shot, from string to value.
   *
   * @param a valid XPath expression - the expression to evaluate (no surrounding braces)
   * @param variables - the variables in scope
   * @param contextNode - the context node for this expression
   * @param namespaces  - the namespaces in scope
   */
  private[xpath] def evalExpressionFromString(expression: String, variables: VariableMap,
    contextNode: Parent, namespaces: Seq[org.jdom.Namespace], targetType: QName = NODE): XPathResult = {

    val compiledExprExceptVariables = compileExpression(expression, namespaces, null) // null as schema component
    val res = evalExpression(expression, compiledExprExceptVariables, variables, contextNode, targetType)
    res
  }

  /**
   * Evaluates an XPath expression that has been compiled.
   *
   * Issues: expressions like { 3 } are not nodes. Asking for a node throws.
   * Asking for a string works, or returns "" on illegal things.
   * If you ask for a number, it will convert to a number or return a NaN on any failure.
   */
  def evalExpression(
    expressionForErrorMsg: String,
    compiledExprFactory: CompiledExpressionFactory,
    variables: VariableMap,
    contextNode: Parent,
    targetType: QName): XPathResult = {
    // withLoggingLevel(LogLevel.Info) 
    {
      val ce = compiledExprFactory.getXPathExpr(variables)
      log(LogLevel.Debug, "Evaluating %s in context %s to get a %s", expressionForErrorMsg, contextNode, targetType) // Careful. contextNode could be null.
      val (isNumeric, newTargetType) = targetType match {
        // Represent numeric types as String to prevent loss of precision
        case NUMBER => (true, STRING)
        case _ => (false, targetType)
      }
      val o = ce.evaluate(contextNode, newTargetType)
      log(LogLevel.Debug, "Evaluated to: %s", o)
      val res =
        try {
          (o, newTargetType) match {
            case (_, NUMBER) => {
              val numStr = o match {
                case x: Element => x.getContent(0).toString()
                case x: Text => x.getValue().toString()
                case x: java.lang.Double if (x.isNaN() && contextNode != null) => {
                  // We got a NaN. If the path actually exists, then the result is a NaN
                  // If the path doesn't exist, then we want to fail.
                  val existingNode = ce.evaluate(contextNode, NODE)
                  if (existingNode != null) x.toString
                  else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
                }
                case x: java.lang.Double => x.toString()
              }
              // Because XPath converts Number to Double, our output strings
              // of the evaluated value (numStr) will always have decimals in them.
              // How can we get around this?
              BigDecimal(numStr) // Will throw exception if not a valid number
              new NumberResult(numStr)
            }
            case (_, STRING) if isNumeric => {
              val numStr = o match {
                case x: Element => x.getContent(0).toString()
                case x: Text => x.getValue()
                case "true" => "1" // Because could evaluate checkConstraints which returns true/false
                case "false" => "0" // Because could evaluate checkConstraints which returns true/false
                case "" if (contextNode != null) => {
                  // We got empty string. If the path actually exists, then the result is empty string.
                  // If the path doesn't exist, then we want to fail.
                  val existingNode = ce.evaluate(contextNode, NODE)
                  if (existingNode != null) throw new XPathExpressionException("unrecognized evaluation result: " + o + " (empty string) for target type " + targetType)
                  else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
                }
                case x: String => x
                case null => throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
                case _ => throw new XPathExpressionException("unrecognized evaluation result: " + o + " for target type " + targetType)
              }
              BigDecimal(numStr)
              new NumberResult(numStr)
            }
            case (x: Element, NODE) => new NodeResult(x)
            case (x: Element, STRING) => new StringResult(x.getContent(0).toString())
            case (x: Text, STRING) => new StringResult(x.getValue())
            case ("", STRING) if (contextNode != null) => {
              // We got empty string. If the path actually exists, then the result is empty string.
              // If the path doesn't exist, then we want to fail.
              val existingNode = ce.evaluate(contextNode, NODE)
              if (existingNode != null) new StringResult("")
              else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
            }
            case (x: String, STRING) => new StringResult(x)
            case (x: java.lang.Boolean, BOOLEAN) => new BooleanResult(x)
            case (null, _) => {
              // There was no such node. We're never going to get an answer for this XPath
              // so fail.
              throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
            }
            case _ => {
              throw new XPathExpressionException("unrecognized evaluation result: " + o + " for target type " + targetType)
            }
          }
        } catch {
          case ex: NumberFormatException => new NotANumberResult(o)
        }
      return res
    }
    // Note: removed "retry looking for a string" code. That was not the right approach to using the
    // XPath API. You don't try for a NODE, and if that fails try for a String. You try for a NODE, and 
    // convert the result to a string if you get a node. (Text IS a node).
  }

  /**
   * Whether a string is a DFDL expression (an XPath expression surrounded by brackets).
   *
   * This function does not verify a string conforms to the DFDL subset of XPath
   */
  def isExpression(expression: String): Boolean =
    expression.startsWith("{") && expression.endsWith("}") &&
      (expression(1) != '{')

  /**
   * Returns the XPath expression contained in a DFDL expression (an XPath expression surrounded by brackets).
   *
   * @param expression a valid DFDL expression
   */
  def getExpression(expression: String): String = {
    val v = expression.trim
    v.substring(1, v.length - 1)
  }

}
