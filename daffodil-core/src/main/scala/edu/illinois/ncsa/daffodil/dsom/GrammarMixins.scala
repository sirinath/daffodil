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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import com.ibm.icu.text.NumberFormat
import java.math.BigInteger

trait GrammarMixin {
  protected val NYI = false // our flag for Not Yet Implemented 
}

trait InitiatedTerminatedMixin
  extends GrammarMixin
  with AnnotatedMixin
  with DelimitedRuntimeValuedPropertiesMixin { self: Term =>

  lazy val parentSaysInitiatedContent = {
    val parentSays = self.immediatelyEnclosingModelGroup match {
      case Some(s) if (s.initiatedContent == YesNo.Yes) => true
      case _ => false
    }
    parentSays
  }

  lazy val hasInitiator = {
    val hasOne = initiator.isKnownNonEmpty
    if (parentSaysInitiatedContent)
      schemaDefinitionUnless(hasOne, "Enclosing group has initiatedContent='yes', but initiator is not defined.")
    hasOne
  }

  lazy val hasTerminator = terminator.isKnownNonEmpty

  lazy val initiatorDiscriminator = Prod("initiatorDiscriminator", this, parentSaysInitiatedContent, prims.InitiatedContent(this))

  lazy val initiatorRegion = Prod("initiatorRegion", this, hasInitiator, initiatorItself ~ initiatorDiscriminator)
  lazy val initiatorItself = {
    if (initiator.isConstant) prims.StaticInitiator(this)
    else prims.DynamicInitiator(this)
  }

  lazy val terminatorRegion = Prod("terminatorRegion", this, hasTerminator,
    if (terminator.isConstant) prims.StaticTerminator(this)
    else prims.DynamicTerminator(this))
}

trait EscapeSchemeRefMixin { self: AnnotatedSchemaComponent =>
  /**
   * Changed to use findProperty, and to resolve the namespace properly.
   *
   * We lookup a property like escapeSchemeRef, and that actual property
   * binding can be local, in scope, by way of a format reference, etc.
   *
   * It's value is a QName, and the definition of the prefix is from the
   * location where we found the property, and NOT where we consume the property.
   *
   * Hence, we resolve w.r.t. the location that provided the property.
   *
   * The point of findProperty vs. getProperty is just that the former returns
   * both the value, and the object that contained it. That object is what
   * we resolve QNames with respect to.
   *
   * Note: Same is needed for properties that have expressions as their values.
   * E.g., consider "{ ../foo:bar/.. }". That foo prefix must be resolved relative
   * to the object where this property was written, not where it is evaluated. (JIRA
   * issue DFDL-77)
   */
  lazy val optionEscapeScheme: Option[DFDLEscapeScheme] = {
    val er = findPropertyOption("escapeSchemeRef")
    er match {
      case _: NotFound => {
        SDW("Property escapeSchemeRef was undefined. Please add escapeSchemeRef='' to your schema.")
        None
      }
      case Found("", _) => None // empty string means no escape scheme
      case Found(qName, loc) => {
        val (nsURI, name) = loc.resolveQName(qName) // loc is where we resolve the QName prefix.
        val defES = schemaSet.getDefineEscapeScheme(nsURI, name)
        defES match {
          case None => SDE("Define Escape Scheme %s Not Found", qName)
          case Some(es) => Some(es.escapeScheme)
        }
      }
    }
  }

}

trait AlignedMixin { self: Term =>
  lazy val leadingSkipRegion = Prod("leadingSkipRegion", this, prims.LeadingSkipRegion(this))
  lazy val trailingSkipRegion = Prod("trailingSkipRegion", this, prims.TrailingSkipRegion(this))
  lazy val alignmentFill = Prod("alignmentFill", this, prims.AlignmentFill(this))
}

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

trait ElementBaseGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin { self: ElementBase =>
  // 
  // This silly redundancy where the variable name has to also be passed as a string,
  // is, by the way, a good reason Scala needs real Lisp-style macros, that can take an argument and
  // turn it into a type/class, object, def, or val/var name, as well as a string, etc. 
  // 

  lazy val parsedNil = Prod("parsedNil", this, NYI && isNillable && nilKind == NilKind.LogicalValue,
    nilElementInitiator ~ prims.LogicalNilValue(this) ~ nilElementTerminator)

  lazy val parsedValue = {
    val res = Prod("parsedValue", this, initiatorRegion ~ allowedValue ~ terminatorRegion)
    res
  }

  def allowedValue: Prod // provided by LocalElementBase for array considerations, and GlobalElementDecl - scalar only

  //  lazy val explicitLengthBinary = Prod("explicitLengthBinary", this, !isFixedLength,
  //    lengthUnits match {
  //      case LengthUnits.Bytes => BinaryExplicitLengthInBytes(this)
  //      case LengthUnits.Characters => schemaDefinitionError("Binary data elements cannot have lengthUnits='Character'.")
  //      case LengthUnits.Bits => BinaryExplicitLengthInBits(this)
  //    })
  //
  //  lazy val binaryValueLength = binaryValueLength_.value
  //  lazy val binaryValueLength_ = LV {
  //    val res = Prod("BinaryValueLength", this, lengthKind match {
  //      case LengthKind.Explicit => explicitLengthBinary
  //      case LengthKind.Delimited => Assert.notYetImplemented() // Binary Data delimiters aren't supported TODO: Should we?
  //      case LengthKind.Pattern => schemaDefinitionError("Binary data elements cannot have lengthKind='Pattern'.")
  //      case LengthKind.Implicit => Assert.notYetImplemented() // TODO: Get size from xs:type
  //      case _ => Assert.notYetImplemented()
  //    })
  //    res
  //  }

  // Length is in bits, (size would be in bytes) (from DFDL Spec 12.3.3)
  lazy val implicitBinaryLengthInBits: Long = primType match {
    case PrimType.Byte | PrimType.UByte => 8
    case PrimType.Short | PrimType.UShort => 16
    case PrimType.Float | PrimType.Int | PrimType.UInt | PrimType.Boolean => 32
    case PrimType.Double | PrimType.Long | PrimType.ULong => 64
    case _ => schemaDefinitionError("Size of binary data '" + primType.name + "' cannot be determined implicitly.")
  }

  lazy val binaryNumberKnownLengthInBits: Long = lengthKind match {
    case LengthKind.Implicit => implicitBinaryLengthInBits
    case LengthKind.Explicit if (length.isConstant) => {
      val lengthFromProp = length.constantAsLong
      val nbits = lengthUnits match {
        case LengthUnits.Bits => lengthFromProp
        case LengthUnits.Bytes => lengthFromProp * 8
        case LengthUnits.Characters => SDE("The lengthUnits for binary numbers must be either 'bits' or 'bytes'. Not 'characters'.")
      }
      nbits
    }
    case LengthKind.Explicit => -1 // means must be computed at runtime.
    case LengthKind.Delimited => schemaDefinitionError("Binary data elements cannot have lengthKind='delimited'.")
    case LengthKind.Pattern => schemaDefinitionError("Binary data elements cannot have lengthKind='pattern'.")
    case LengthKind.Prefixed => subsetError("lengthKind='prefixed' not yet supported.")
    case LengthKind.EndOfParent => schemaDefinitionError("Binary data elements cannot have lengthKind='endOfParent'.")
  }

  lazy val fixedLengthString = Prod("fixedLengthString", this, isFixedLength,
    (lengthUnits, knownEncodingIsFixedWidth) match {
      case (LengthUnits.Bytes, true) => prims.StringFixedLengthInBytesFixedWidthCharacters(this, fixedLength) // TODO: make sure it divides evenly.
      //case (LengthUnits.Bytes, true) => prims.StringFixedLengthInBytes(this, fixedLength / knownEncodingWidth) // TODO: make sure it divides evenly.
      case (LengthUnits.Bytes, false) => prims.StringFixedLengthInBytesVariableWidthCharacters(this, fixedLength)
      case (LengthUnits.Characters, true) => {
        //
        // we deal with the fact that some encodings have characters taking up smaller than 
        // a full byte. E.g., encoding='US-ASCII-7-bit-packed' are 7-bits packed with no unused
        // bits
        //
        val lengthInBits = fixedLength * knownEncodingWidthInBits
        val lengthInBytes = lengthInBits / 8
        val hasWholeBytesOnly = (lengthInBits % 8) == 0
        if (hasWholeBytesOnly)
          prims.StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes)
        else
          prims.StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes + 1) // 1 more for fragment byte
      }
      //
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => prims.StringFixedLengthInVariableWidthCharacters(this, fixedLength)
      case (LengthUnits.Bits, _) => notYetImplemented("lengthUnits='bits' for type " + typeDef)
      case _ => Assert.invariantFailed("all cases should have been exhausted.")
    })

  lazy val fixedLengthHexBinary = Prod("fixedLengthHexBinary", this, isFixedLength,
    lengthUnits match {
      case LengthUnits.Bytes => prims.HexBinaryFixedLengthInBytes(this, fixedLength)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    })

  lazy val implicitLengthString = Prod("implicitLengthString", this, hasSpecifiedLength,
    (lengthUnits, knownEncodingIsFixedWidth) match {
      case (LengthUnits.Bytes, true) => prims.StringFixedLengthInBytesFixedWidthCharacters(this, facetMaxLength) // TODO: make sure it divides evenly.
      //case (LengthUnits.Bytes, true) => prims.StringFixedLengthInBytes(this, fixedLength / knownEncodingWidth) // TODO: make sure it divides evenly.
      case (LengthUnits.Bytes, false) => prims.StringFixedLengthInBytesVariableWidthCharacters(this, facetMaxLength)
      case (LengthUnits.Characters, true) => {
        //
        // we deal with the fact that some encodings have characters taking up smaller than 
        // a full byte. E.g., encoding='US-ASCII-7-bit-packed' are 7-bits packed with no unused
        // bits
        //
        val lengthInBits = facetMaxLength * knownEncodingWidthInBits
        val lengthInBytes = lengthInBits / 8
        val hasWholeBytesOnly = (lengthInBits % 8) == 0
        if (hasWholeBytesOnly)
          prims.StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes)
        else
          prims.StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes + 1) // 1 more for fragment byte
      }
      //
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => prims.StringFixedLengthInVariableWidthCharacters(this, facetMaxLength)
      case (LengthUnits.Bits, _) => SDE("Strings with lengthKind='implicit' may not have lengthUnits='bits'")
    })

  lazy val implicitLengthHexBinary = Prod("implicitLengthHexBinary", this, hasSpecifiedLength,
    lengthUnits match {
      case LengthUnits.Bytes => prims.HexBinaryFixedLengthInBytes(this, facetMaxLength)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    })

  lazy val variableLengthString = Prod("variableLengthString", this, !isFixedLength,
    (lengthUnits, knownEncodingIsFixedWidth) match {
      //case (LengthUnits.Bytes, true) => StringExplicitLengthInBytes(this)
      //case (LengthUnits.Bytes, false) =>
      //  notYetImplemented("lengthKind='explicit' and lengthUnits='bytes' with non-fixed-width or potentially non-fixed-width encoding='%s'.", this.encodingRaw)
      //// StringExplicitLengthInBytesVariableWidthCharacters(this)
      //case (LengthUnits.Characters, _) =>
      // notYetImplemented("lengthKind='explicit' and lengthUnits='characters'")
      //// Above, keep in mind fixed length but variable-width encoding means variable width.
      //// The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      //// 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      //case (LengthUnits.Bits, _) =>
      //  SDE("lengthKind='explicit' and lengthUnits='bits' for type %s", this.typeDef)
      case (LengthUnits.Bytes, true) => prims.StringVariableLengthInBytes(this)
      case (LengthUnits.Bytes, false) => prims.StringVariableLengthInBytesVariableWidthCharacters(this)
      case (LengthUnits.Characters, true) => prims.StringVariableLengthInBytes(this)
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => prims.StringVariableLengthInVariableWidthCharacters(this)
      case (LengthUnits.Bits, _) => SDE("lengthKind='explicit' and lengthUnits='bits' for type %s", this.typeDef)
    })

  lazy val variableLengthHexBinary = Prod("variableLengthHexBinary", this, !isFixedLength,
    lengthUnits match {
      case LengthUnits.Bytes => prims.HexBinaryVariableLengthInBytes(this)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    })

  //lazy val stringDelimitedEndOfData = Prod("stringDelimitedEndOfData", this, StringDelimitedEndOfData(this))
  lazy val stringDelimitedEndOfDataStatic = Prod("stringDelimitedEndOfDataStatic", this, prims.StringDelimitedEndOfDataStatic(this))
  lazy val stringDelimitedEndOfDataDynamic = Prod("stringDelimitedEndOfDataDynamic", this, prims.StringDelimitedEndOfDataDynamic(this))
  lazy val stringPatternMatched = Prod("stringPatternMatched", this, prims.StringPatternMatched(this))

  lazy val stringValue = stringValue_.value
  private val stringValue_ = LV('stringValue) {
    val res = Prod("stringValue", this, lengthKind match {
      case LengthKind.Explicit if isFixedLength => fixedLengthString
      case LengthKind.Explicit => variableLengthString
      //case LengthKind.Delimited => stringDelimitedEndOfData
      case LengthKind.Delimited if this.hasExpressionsInTerminatingMarkup => stringDelimitedEndOfDataDynamic
      case LengthKind.Delimited => stringDelimitedEndOfDataStatic
      case LengthKind.Pattern => stringPatternMatched
      case LengthKind.Implicit => {
        val pt = this.simpleType.primitiveType
        Assert.invariant(pt == PrimType.String)
        implicitLengthString
      }
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    })
    res
  }

  lazy val hexBinaryDelimitedEndOfDataStatic = Prod("hexBinaryDelimitedEndOfDataStatic", this, prims.HexBinaryDelimitedEndOfDataStatic(this))
  lazy val hexBinaryDelimitedEndOfDataDynamic = Prod("hexBinaryDelimitedEndOfDataDynamic", this, prims.HexBinaryDelimitedEndOfDataDynamic(this))

  lazy val hexBinaryValue = hexBinaryValue_.value
  private val hexBinaryValue_ = LV('hexBinaryValue) {
    val res = Prod("hexBinaryValue", this, lengthKind match {
      case LengthKind.Explicit if isFixedLength => fixedLengthHexBinary
      case LengthKind.Explicit => variableLengthHexBinary
      case LengthKind.Delimited if this.hasExpressionsInTerminatingMarkup => hexBinaryDelimitedEndOfDataDynamic
      case LengthKind.Delimited => hexBinaryDelimitedEndOfDataStatic
      case LengthKind.Pattern => SDE("lengthKind Pattern is not allowed for hexBinary.")
      case LengthKind.Implicit => implicitLengthHexBinary
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    })
    res
  }

  //  lazy val binaryByte = Prod("binaryByte", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryShort = Prod("binaryShort", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryLong = Prod("binaryLong", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryInteger = Prod("binaryInteger", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedInt = Prod("binaryInt", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedByte = Prod("binaryByte", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedShort = Prod("binaryShort", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedLong = Prod("binaryLong", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)

  //  lazy val regularBinaryRepInt = Prod("regularBinaryRepInt", this,
  //    binaryNumberRep == BinaryNumberRep.Binary, lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) {
  //          val javaByteOrder = ByteOrder(byteOrder.constantAsString, this) match {
  //            case ByteOrder.BigEndian => java.nio.ByteOrder.BigEndian
  //            case ByteOrder.LittleEndian => java.nio.ByteOrder.LittleEndian
  //          }
  //          Regular32bitIntPrim(this, javaByteOrder)
  //        else Assert.notYetImplemented() // Dynamic byte order not implemented
  //      }
  //      case _ => Assert.notYetImplemented() // binary number length kinds other than implicit not implemented
  //    })

  //  lazy val bcdInt = Prod("bcdInt", this,
  //    binaryNumberRep == BinaryNumberRep.Bcd, BCDIntPrim(this))
  //  lazy val packedInt = Prod("packedInt", this,
  //    binaryNumberRep == BinaryNumberRep.Packed, PackedIntPrim(this))

  // TODO: Handle the zonedTextXXX possibilities
  lazy val textInt = Prod("textInt", this, representation == Representation.Text,
    standardTextInt | zonedTextInt)

  lazy val textByte = Prod("textByte", this, representation == Representation.Text,
    standardTextByte | zonedTextInt)

  lazy val textShort = Prod("textShort", this, representation == Representation.Text,
    standardTextShort | zonedTextInt)

  lazy val textLong = Prod("textLong", this, representation == Representation.Text,
    standardTextLong | zonedTextInt)

  lazy val textInteger = Prod("textInteger", this, representation == Representation.Text,
    standardTextInteger | zonedTextInt)

  lazy val textNonNegativeInteger = Prod("textNonNegativeInteger", this, representation == Representation.Text,
    standardTextNonNegativeInteger | zonedTextInt)

  lazy val textUnsignedInt = Prod("textUnsignedInt", this, representation == Representation.Text,
    standardTextUnsignedInt | zonedTextInt)

  lazy val textUnsignedByte = Prod("textUnsignedByte", this, representation == Representation.Text,
    standardTextUnsignedByte | zonedTextInt)

  lazy val textUnsignedShort = Prod("textUnsignedShort", this, representation == Representation.Text,
    standardTextUnsignedShort | zonedTextInt)

  lazy val textUnsignedLong = Prod("textUnsignedLong", this, representation == Representation.Text,
    standardTextUnsignedLong | zonedTextInt)

  //
  // We could now break it down by lengthKind, and have specialized primitives
  // depending on the length kind.
  // 
  lazy val standardTextInteger = Prod("standardTextInteger", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextIntegerPrim(this))
  lazy val standardTextNonNegativeInteger = Prod("standardTextNonNegativeInteger", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextNonNegativeIntegerPrim(this))
  lazy val standardTextLong = Prod("standardTextLong", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextLongPrim(this))
  lazy val standardTextInt = Prod("standardTextInt", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextIntPrim(this))
  lazy val standardTextShort = Prod("standardTextShort", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextShortPrim(this))
  lazy val standardTextByte = Prod("standardTextByte", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextBytePrim(this))
  lazy val standardTextUnsignedLong = Prod("standardTextUnsignedLong", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextUnsignedLongPrim(this))
  lazy val standardTextUnsignedInt = Prod("standardTextUnsignedInt", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextUnsignedIntPrim(this))
  lazy val standardTextUnsignedShort = Prod("standardTextUnsignedShort", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextUnsignedShortPrim(this))
  lazy val standardTextUnsignedByte = Prod("standardTextUnsignedByte", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextUnsignedBytePrim(this))
  lazy val zonedTextInt = Prod("zonedTextInt", this,
    textNumberRep == TextNumberRep.Zoned, prims.ZonedTextIntPrim(this))

  //  lazy val binaryDouble = Prod("binaryDouble", this, representation == Representation.Binary,
  //    ieeeBinaryRepDouble | ibm390HexBinaryRepDouble)

  lazy val textDouble = Prod("textDouble", this, representation == Representation.Text,
    standardTextDouble | zonedTextDouble)

  //  lazy val ieeeBinaryRepDouble = Prod("ieeeBinaryRepDouble", this,
  //    {
  //      val bfr = binaryFloatRep
  //      val res = bfr.isConstant &&
  //        BinaryFloatRep(bfr.constantAsString, this) == BinaryFloatRep.Ieee
  //      res
  //    },
  //    lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) ByteOrder(byteOrder.constantAsString, this) match {
  //          case ByteOrder.BigEndian => BigEndianDoublePrim(this)
  //          case ByteOrder.LittleEndian => LittleEndianDoublePrim(this)
  //        }
  //        else Assert.notYetImplemented()
  //      }
  //      case _ => Assert.notYetImplemented()
  //    })

  lazy val ibm390HexBinaryRepDouble = Prod("ibm390HexBinaryRepDouble", this,
    binaryFloatRep.isConstant &&
      binaryFloatRep.constantAsString == BinaryFloatRep.Ibm390Hex.toString,
    subsetError("ibm390Hex not supported"))

  lazy val standardTextDouble = Prod("standardTextDouble", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextDoublePrim(this))

  lazy val zonedTextDouble = Prod("zonedTextDouble", this,
    textNumberRep == TextNumberRep.Zoned, SDE("Zoned not supported for float and double"))

  //  lazy val binaryFloat = Prod("binaryFloat", this, representation == Representation.Binary,
  //    ieeeBinaryRepFloat | ibm390HexBinaryRepFloat)

  lazy val textFloat = Prod("textFloat", this, representation == Representation.Text,
    standardTextFloat | zonedTextFloat)

  //  lazy val ieeeBinaryRepFloat = Prod("ieeeBinaryRepFloat", this,
  //    {
  //      val bfr = binaryFloatRep
  //      val res = bfr.isConstant &&
  //        BinaryFloatRep(bfr.constantAsString, this) == BinaryFloatRep.Ieee
  //      res
  //    },
  //    lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) ByteOrder(byteOrder.constantAsString, this) match {
  //          case ByteOrder.BigEndian => BigEndianFloatPrim(this)
  //          case ByteOrder.LittleEndian => LittleEndianFloatPrim(this)
  //        }
  //        else Assert.notYetImplemented()
  //      }
  //      case _ => Assert.notYetImplemented()
  //    })
  //
  //  lazy val ibm390HexBinaryRepFloat = Prod("ibm390HexBinaryRepFloat", this,
  //    binaryFloatRep.isConstant &&
  //      binaryFloatRep.constantAsString == BinaryFloatRep.Ibm390Hex.toString,
  //    subsetError("ibm390Hex not supported"))

  lazy val standardTextFloat = Prod("standardTextFloat", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ prims.ConvertTextFloatPrim(this))

  lazy val zonedTextFloat = Prod("zonedTextFloat", this,
    textNumberRep == TextNumberRep.Zoned, SDE("Zoned not supported for float and double"))

  lazy val textDate = Prod("textDate", this, representation == Representation.Text,
    stringValue ~ prims.ConvertTextDatePrim(this))
  lazy val textTime = Prod("textTime", this, representation == Representation.Text,
    stringValue ~ prims.ConvertTextTimePrim(this))
  lazy val textDateTime = Prod("textDateTime", this, representation == Representation.Text,
    stringValue ~ prims.ConvertTextDateTimePrim(this))

  // shorthand
  lazy val primType = {
    val res = typeDef.asInstanceOf[SimpleTypeBase].primitiveType
    res
  }

  //  lazy val value = Prod("value", this, isSimpleType,
  //    // TODO: Consider issues with matching a stopValue. Can't say isScalar here because
  //    // This gets used for array contents also.
  //    {
  //      primType.name match {
  //        case "string" => stringValue
  //        case _ => {
  //          val res = representation match {
  //            case Representation.Binary => binaryValue
  //            case Representation.Text => textValue
  //          }
  //          res
  //        }
  //      }
  //    })

  lazy val value = Prod("value", this, isSimpleType,
    // TODO: Consider issues with matching a stopValue. Can't say isScalar here because
    // this gets used for array contents also.
    {
      primType match {
        case PrimType.String => stringValue
        case PrimType.HexBinary => hexBinaryValue
        case _ => {
          val res = representation match {
            case Representation.Binary => binaryValue
            case Representation.Text => textValue
          }
          res
        }
      }
    })

  // This is the right name that the DFDL property should have had!
  lazy val binaryIntRep = {
    subset(binaryNumberRep == BinaryNumberRep.Binary, "binaryNumberRep='%s' is unsupported. Only 'binary' is supported.", binaryNumberRep.toString)
    binaryNumberRep
  }

  lazy val staticBinaryFloatRep = {
    subset(binaryFloatRep.isConstant, "Dynamic binaryFloatRep is not supported.")
    BinaryFloatRep(binaryFloatRep.constantAsString, this)
  }

  lazy val binary = {
    subset(lengthKind == LengthKind.Explicit, "Currently only lengthKind='explicit' is supported.")
    LengthKind(lengthKind.toString(), this)
  }

  val bin = BinaryNumberRep.Binary // shorthands for table dispatch
  val ieee = BinaryFloatRep.Ieee
  type BO = java.nio.ByteOrder

  lazy val zero = new BigInteger("0")
  lazy val two = new BigInteger("2")
  lazy val maximumUnsignedLong = two.pow(64).subtract(new BigInteger("1"))

  lazy val binaryValue: Gram = {
    Assert.invariant(primType != PrimType.String)

    subset(byteOrder.isConstant, "Dynamic byte order is not currently supported.")

    // We have to dispatch carefully here. We cannot force evaluation of properties 
    // that may not be necessary. E.g., float does not need property binaryNumberRep, so
    // if our dispatch table uses that, it will create a false dependency on the property
    // being defined. 
    // The DFDL spec has a section where it gives the precedence order of properties. 
    // This is in the spirit of that section.
    val res: Gram = primType match {

      //      case PrimType.HexBinary =>
      //        (primType, binary) match { // TODO: Only takes explicit length
      //          case (PrimType.HexBinary, b) => new BinaryNumberBase[Array[Byte]](this, this.length.constantAsLong) {
      //            def getNum(bp : Long, in : InStream, bo : BO) = {
      //              // FIXME: size constraints, overflow
      //              in.getByteArray(bp, bo, length.constantAsLong.asInstanceOf[Int])
      //            }
      //            override def getNum(num : Number) = null //FIXME
      //            protected override val GramName = "hexBinary"
      //            protected override val GramDescription = "Hex Binary"
      //            protected override def numFormat = NumberFormat.getIntegerInstance()
      //            protected override def isInt = true
      //          }
      //          case _ => Assert.impossibleCase()
      //        }

      case PrimType.Byte | PrimType.Short | PrimType.Int | PrimType.Long | PrimType.Integer => {
        Assert.invariant(binaryIntRep == bin)
        binaryNumberKnownLengthInBits match {
          case -1 => prims.SignedRuntimeLengthRuntimeByteOrderBinaryNumber(this)
          case _ => prims.SignedKnownLengthRuntimeByteOrderBinaryNumber(this, binaryNumberKnownLengthInBits)
        }
      }

      case PrimType.UByte | PrimType.UShort | PrimType.UInt | PrimType.ULong | PrimType.NonNegativeInteger => {
        Assert.invariant(binaryIntRep == bin)
        binaryNumberKnownLengthInBits match {
          case -1 => prims.UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber(this)
          case _ => prims.UnsignedKnownLengthRuntimeByteOrderBinaryNumber(this, binaryNumberKnownLengthInBits)
        }
      }

      case PrimType.Double | PrimType.Float =>
        (primType, binaryNumberKnownLengthInBits, staticBinaryFloatRep) match {
          case (_, -1, BinaryFloatRep.Ieee) => SDE("Floating point binary numbers may not have runtime-specified lengths.")
          case (PrimType.Float, 32, BinaryFloatRep.Ieee) => prims.FloatKnownLengthRuntimeByteOrderBinaryNumber(this, 32)
          case (PrimType.Float, n, BinaryFloatRep.Ieee) => SDE("binary xs:float must be 32 bits. Length in bits was %s.", n)
          case (PrimType.Double, 64, BinaryFloatRep.Ieee) => prims.DoubleKnownLengthRuntimeByteOrderBinaryNumber(this, 64)
          case (PrimType.Double, n, BinaryFloatRep.Ieee) => SDE("binary xs:double must be 64 bits. Length in bits was %s.", n)
          case (_, _, floatRep) => subsetError("binaryFloatRep='%s' not supported. Only binaryFloatRep='ieee'", floatRep.toString)
        }

      //        (primType, staticBinaryFloatRep) match {
      //          case (PrimType.Double, ieee) => new BinaryNumber[Double](this, 64) {
      //            Assert.invariant(staticBinaryFloatRep == BinaryFloatRep.Ieee)
      //            def getNum(bp : Long, in : InStream, bo : BO) = in.getDouble(bp, bo)
      //            override def getNum(num : Number) = num.doubleValue
      //            protected override val GramName = "double"
      //            protected override val GramDescription = "Double"
      //            protected override def numFormat = NumberFormat.getNumberInstance() // .getScientificInstance() Note: scientific doesn't allow commas as grouping separators.
      //            protected override def isInt = false
      //          }
      //          case (PrimType.Float, ieee) => new BinaryNumber[Float](this, 32) {
      //            Assert.invariant(staticBinaryFloatRep == BinaryFloatRep.Ieee)
      //            def getNum(bp : Long, in : InStream, bo : BO) = in.getFloat(bp, bo)
      //            override def getNum(num : Number) = num.floatValue
      //            protected override val GramName = "float"
      //            protected override val GramDescription = "Float"
      //            protected override def numFormat = NumberFormat.getNumberInstance() // .getScientificInstance() Note: scientific doesn't allow commas as grouping separators.
      //            protected override def isInt = false
      //          }
      //          case (_, floatRep) => subsetError("binaryFloatRep='%s' not supported. Only binaryFloatRep='ieee'", floatRep.toString)
      //        }
      case _ => notYetImplemented("Type %s when representation='binary'", primType.name)
    }
    res
  }

  lazy val textValue: Gram = {
    val pt = primType
    Assert.invariant(pt != PrimType.String)
    Assert.invariant(pt != PrimType.HexBinary)
    Assert.invariant(representation == Representation.Text)
    schemaDefinitionWhen(lengthKind == LengthKind.Implicit,
      "Type %s cannot have lengthKind='implicit' when representation='text'",
      pt.name)
    val res = primType match {
      case PrimType.Int => textInt
      case PrimType.Byte => textByte
      case PrimType.Short => textShort
      case PrimType.Long => textLong
      case PrimType.Integer => textInteger
      case PrimType.UInt => textUnsignedInt
      case PrimType.UByte => textUnsignedByte
      case PrimType.UShort => textUnsignedShort
      case PrimType.ULong => textUnsignedLong
      case PrimType.NonNegativeInteger => textNonNegativeInteger // Should be treated as unsigned xs:integer
      case PrimType.Double => textDouble
      case PrimType.Float => textFloat
      case PrimType.HexBinary => Assert.invariantFailed("Primitive hexBinary must be representation='binary'.")
      case PrimType.Boolean => notYetImplemented("textValue: boolean")
      case PrimType.Date => textDate
      case PrimType.Time => textTime
      case PrimType.DateTime => textDateTime
      case _ => schemaDefinitionError("Unrecognized primitive type: " + primType.name)
    }
    res
  }

  lazy val empty = Prod("empty", this, NYI && emptyIsAnObservableConcept, emptyRepresentation)

  lazy val emptyRepresentation = Prod("emptyRepresentation", this,
    simpleOrNonImplicitComplexEmpty | complexImplicitEmpty)

  lazy val simpleOrNonImplicitComplexEmpty = Prod("simpleOrNonImplicitComplexEmpty", this,
    NYI && isSimpleType || isComplexType && lengthKind != LengthKind.Implicit,
    emptyElementInitiator ~ emptyElementTerminator)

  /**
   * This is about the case where we take an empty, parse a complex type recursively from it
   * and potentially succeed.
   */
  lazy val complexImplicitEmpty = Prod("complexImplicitEmpty", this, NYI &&
    isComplexType && lengthKind == LengthKind.Implicit,
    prims.SaveInputStream(this) ~ prims.SetEmptyInputStream(this) ~ elementComplexType.mainGrammar ~
      prims.RestoreInputStream(this) ~ emptyElementTerminator)

  lazy val emptyDefaulted = Prod("emptyDefaulted", this,
    isDefaultable && emptyIsAnObservableConcept,
    empty ~ prims.TheDefaultValue(this))

  lazy val nilElementInitiator = Prod("nilElementInitiator", this, hasNilValueInitiator,
    if (initiator.isConstant) prims.StaticInitiator(this) else prims.DynamicInitiator(this))
  lazy val nilElementTerminator = Prod("nilElementTerminator", this, hasNilValueTerminator,
    if (terminator.isConstant) prims.StaticTerminator(this) else prims.DynamicTerminator(this))

  lazy val emptyElementInitiator = Prod("emptyElementInitiator", this, NYI && hasEmptyValueInitiator, EmptyGram)
  lazy val emptyElementTerminator = Prod("emptyElementTerminator", this, NYI && hasEmptyValueTerminator, EmptyGram)

  lazy val complexContent = Prod("complexContent", this, isComplexType,
    initiatorRegion ~ elementComplexType.mainGrammar ~ terminatorRegion)

  lazy val nilLit = {
    Prod("nilLit", this,
      isNillable && nilKind == NilKind.LiteralValue,
      nilElementInitiator ~ {
        // if (representation != Representation.Text) this.SDE("LiteralValue Nils require representation='text'.")
        lengthKind match {
          //          case LengthKind.Delimited => LiteralNilDelimitedOrEndOfData(this)
          case LengthKind.Delimited if this.hasExpressionsInTerminatingMarkup => prims.LiteralNilDelimitedEndOfDataDynamic(this)
          case LengthKind.Delimited => prims.LiteralNilDelimitedEndOfDataStatic(this)
          case LengthKind.Pattern => prims.LiteralNilPattern(this)
          case LengthKind.Explicit => {
            lengthUnits match {
              case LengthUnits.Bits => notYetImplemented("nilKind='literalValue' with lengthKind='bits'")
              case LengthUnits.Bytes => prims.LiteralNilExplicitLengthInBytes(this)
              case LengthUnits.Characters => prims.LiteralNilExplicitLengthInChars(this)
            }
          }
          case LengthKind.Implicit => {
            schemaDefinitionUnless(representation != Representation.Text, "LiteralValue Nils with lengthKind='implicit' cannot have representation='text'.")
            val lengthInBytes = implicitBinaryLengthInBits / 8
            prims.LiteralNilKnownLengthInBytes(this, lengthInBytes)
          }
          case LengthKind.Prefixed => notYetImplemented("lengthKind='prefixed'")
          case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent'")
        }
      } ~ nilElementTerminator)
  }

  lazy val scalarDefaultableSimpleContent = {
    val res = Prod("scalarDefaultableSimpleContent", this,
      isSimpleType, nilLit | emptyDefaulted | parsedNil | parsedValue)
    res
  }

  lazy val scalarNonDefaultSimpleContent = {
    val res = Prod("scalarNonDefaultSimpleContent", this,
      isSimpleType, nilLit | parsedNil | parsedValue)
    res
  }

  def specifiedLength(body: => Gram) = {
    lengthKind match {
      case LengthKind.Pattern => prims.SpecifiedLengthPattern(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bits && isFixedLength => prims.SpecifiedLengthExplicitBitsFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bits && !isFixedLength => prims.SpecifiedLengthExplicitBits(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bytes && isFixedLength => prims.SpecifiedLengthExplicitBytesFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bytes && !isFixedLength => prims.SpecifiedLengthExplicitBytes(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Characters && isFixedLength => prims.SpecifiedLengthExplicitCharactersFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Characters && !isFixedLength => prims.SpecifiedLengthExplicitCharacters(this, body)
      case _ => {
        // TODO: implement other specified length restrictions
        // for now, no restriction
        body
      }
    }

  }

  lazy val scalarComplexContent = Prod("scalarComplexContent", this, isComplexType, specifiedLength(nilLit | complexContent))

  // Note: there is no such thing as defaultable complex content because you can't have a 
  // default value for a complex type element.
  lazy val scalarDefaultableContent = Prod("scalarDefaultableContent", this, scalarDefaultableSimpleContent | scalarComplexContent)

  lazy val scalarNonDefaultContent = Prod("scalarNonDefaultContent", this, scalarNonDefaultSimpleContent | scalarComplexContent)

  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  lazy val elementLeftFraming = Prod("elementLeftFraming", this,
    leadingSkipRegion ~ alignmentFill ~ prims.PrefixLength(this))

  lazy val elementRightFraming = Prod("elementRightFraming", this, trailingSkipRegion)

  lazy val dfdlElementBegin = Prod("dfdlElementBegin", this, prims.ElementBegin(this))

  lazy val dfdlElementEnd = Prod("dfdlElementEnd", this, {
    if (isRepresented) prims.ElementEnd(this)
    else prims.ElementEndNoRep(this)
  })

  //  lazy val scalarNonDefault = Prod("scalarNonDefault", this,
  //    dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
  //      scalarNonDefaultContent ~ elementRightFraming ~ dfdlStatementEvaluations ~ dfdlScopeEnd ~ dfdlElementEnd)

  lazy val scalarNonDefaultPhysical = Prod("scalarNonDefault", this,
    prims.StmtEval(this, dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
      scalarNonDefaultContent) ~ elementRightFraming ~ dfdlScopeEnd ~ dfdlElementEnd)

  //  def scalarDefaultable: Prod
  //
  //  def scalarNonDefault: Prod
  lazy val scalarDefaultable = Prod("scalarDefaultable", this,
    inputValueCalcOption match {
      case _: NotFound => scalarDefaultablePhysical
      case _: Found => inputValueCalcElement
    })

  lazy val scalarNonDefault = Prod("scalarNonDefault", this,
    inputValueCalcOption match {
      case _: NotFound => scalarNonDefaultPhysical
      case _: Found => inputValueCalcElement
    })

  lazy val inputValueCalcElement = Prod("inputValueCalcElement", this,
    isSimpleType && inputValueCalcOption.isInstanceOf[Found],
    prims.StmtEval(this, dfdlElementBegin ~ dfdlScopeBegin ~
      prims.InputValueCalc(self)) ~ dfdlScopeEnd ~ dfdlElementEnd)

  lazy val scalarDefaultablePhysical = Prod("scalarDefaultablePhysical", this, {
    val res = prims.StmtEval(this, dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
      scalarDefaultableContent) ~ elementRightFraming ~ dfdlScopeEnd ~ dfdlElementEnd
    res
  })

}

trait ElementReferenceGrammarMixin { self: ElementRef =>
  override lazy val termContentBody = self.referencedElement.termContentBody
}

trait LocalElementGrammarMixin { self: LocalElementBase =>

  lazy val termContentBody = {
    val res = Prod("termContentBody", self, separatedScalarDefaultable | recurrance)
    res
  }

  lazy val allowedValue = Prod("allowedValue", this, notStopValue | value)

  lazy val notStopValue = Prod("notStopValue", this, hasStopValue, prims.NotStopValue(this))

  lazy val separatedEmpty = Prod("separatedEmpty", this, emptyIsAnObservableConcept, separatedForPosition(empty))
  lazy val separatedScalarDefaultable = Prod("separatedScalarDefaultable", this, isScalar, separatedForPosition(scalarDefaultable))
  lazy val separatedRecurringDefaultable = Prod("separatedRecurringDefaultable", this, !isScalar, separatedForPosition(scalarDefaultable))
  lazy val separatedScalarNonDefault = Prod("separatedScalarNonDefault", this, isScalar, separatedForPosition(scalarNonDefault))
  lazy val separatedRecurringNonDefault = Prod("separatedRecurringNonDefault", this, !isScalar, separatedForPosition(scalarNonDefault))

  lazy val nonSeparatedScalarDefaultable = Prod("nonSeparatedScalarDefaultable", this, isScalar, scalarDefaultable)

  lazy val recurrance = Prod("recurrance", this,
    !isScalar,
    prims.StartArray(this) ~ arrayContents ~ prims.EndArray(this) ~ prims.FinalUnusedRegion(this))

  override lazy val asTermInChoice = {
    val res = Prod("asTermInChoice", this, nonSeparatedScalarDefaultable | recurrance)
    res
  }

  /**
   * speculate parsing forward until we get an error
   */
  lazy val separatedContentUnboundedWithoutTrailingEmpties = Prod("separatedContentUnboundedWithoutTrailingEmpties", this, isRecurring,
    prims.RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      prims.RepUnbounded(self, separatedRecurringNonDefault) ~
      prims.StopValue(this))

  lazy val separatedContentUnbounded = Prod("separatedContentUnbounded", this, isRecurring,
    separatedContentUnboundedWithoutTrailingEmpties // These are for tolerating trailing empties. Let's not tolerate them for now.
    //        ~
    //        RepUnbounded(separatedEmpty)
    )

  lazy val separatedContentAtMostNWithoutTrailingEmpties = Prod("separatedContentAtMostNWithoutTrailingEmpties", this, isRecurring,
    prims.RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      prims.RepAtMostTotalN(this, maxOccurs, separatedRecurringNonDefault) ~
      prims.StopValue(this))

  // TODO: Do we have to adjust the count to take stopValue into account?
  // Answer: No because the counts are never used when there is a stopValue (at least in current
  // thinking about how occursCountKind='stopValue' works.)

  lazy val separatedContentAtMostN = Prod("separatedContentAtMostN", this, isRecurring,
    separatedContentAtMostNWithoutTrailingEmpties ~
      prims.RepAtMostTotalN(self, maxOccurs, separatedEmpty)) // absorb extra separators, if found.

  /**
   *  parse counted number of occurrences exactly.
   */
  lazy val stopValueSize = if (hasStopValue) 1 else 0

  // TODO FIXME: We really want to have different productions for parsing and unparsing in these
  // complex cases where there is defaulting, etc. Unparsing has many fewer cases, and is just not
  // symmetric with parsing in these situations.
  def separatedContentExactlyN(count: Long) = {
    prims.RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      prims.RepAtMostTotalN(self, count, separatedRecurringNonDefault) ~
      prims.StopValue(this) ~
      prims.RepExactlyTotalN(self, maxOccurs + stopValueSize, separatedEmpty) // absorb reps remaining separators
  }

  lazy val separatedContentExactlyNComputed = {
    prims.OccursCountExpression(this) ~
      prims.RepAtMostOccursCount(this, minOccurs, separatedRecurringDefaultable) ~
      prims.RepExactlyTotalOccursCount(this, separatedRecurringNonDefault)
  }

  // keep in mind that anything here that scans for a representation either knows the length it is going after, or knows what the terminating markup is, and
  // our invariant is, that it does NOT consume that markup ever. The parser consumes it with appropriate grammar terminals. 

  val UNB = -1 // UNBOUNDED
  val ZERO = 0 // ZERO

  lazy val arrayContents = {
    val res = Prod("arrayContents", this, isRecurring,
      arrayContentsNoSeparators | arrayContentsWithSeparators)
    res
  }

  lazy val contentUnbounded = {

    val res = Prod("contentUnbounded", this, isRecurring, prims.RepUnbounded(self, separatedRecurringDefaultable))
    res
  }

  //
  // Silly constants to make the lookup tables below more readable without using fragile whitespace
  //
  final val Never______ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.Never
  final val Trailing___ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingEmpty
  final val TrailingStr: SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingEmptyStrict
  final val Always_____ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.AnyEmpty

  final val StopValue_ = OccursCountKind.StopValue
  final val Implicit__ = OccursCountKind.Implicit
  final val Parsed____ = OccursCountKind.Parsed
  final val Fixed_____ = OccursCountKind.Fixed
  final val Expression = OccursCountKind.Expression

  lazy val arrayContentsNoSeparators = Prod("arrayContentsNoSeparators", this, isRecurring && !hasSep, {
    val res = (occursCountKind, minOccurs, maxOccurs) match {
      case (Expression, ____, __2) => separatedContentExactlyNComputed
      case (Fixed_____, ____, UNB) => SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case (Fixed_____, min_, max) if (min_ != max) => SDE("occursCountKind='fixed' requires minOccurs and maxOccurs to be equal (%d != %d)", min_, max)
      case (Fixed_____, ____, max) => separatedContentExactlyN(max)
      case (Implicit__, ZERO, UNB) => contentUnbounded // same as parsed
      case (Implicit__, min_, UNB) => prims.RepExactlyN(self, min_, separatedRecurringDefaultable) ~ contentUnbounded // respects minOccurs      
      case (Implicit__, ____, __2) => separatedContentAtMostN // uses min and maxOccurs
      case (Parsed____, ____, __2) => contentUnbounded
      case (StopValue_, ____, __2) => contentUnbounded
    }
    res
  })

  /**
   * Matches the table about separator suppression policy.
   *
   * TODO: Right now that table is in DFDL WG subgroup working on "Issue 140" which is trying to
   * rationalize separator suppression among other things. Update this table to match the final spec.
   */
  lazy val arrayContentsWithSeparators = Prod("arrayContentsWithSeparators", this, isRecurring && hasSep, {
    val triple = (separatorSuppressionPolicy, occursCountKind, maxOccurs, minOccurs)
    val res = triple match {
      case (___________, Expression, ___, __2) => separatedContentExactlyNComputed
      case (___________, Fixed_____, UNB, ___) => SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case (___________, Fixed_____, max, min) if (max != min) => SDE("occursCountKind='fixed' requires minOccurs to equal maxOccurs (%d != %d)", minOccurs, max)
      case (___________, Fixed_____, max, ___) => separatedContentExactlyN(max)
      case (Never______, Implicit__, UNB, ___) => SDE("separatorSuppressionPolicy='never' with occursCountKind='implicit' required bounded maxOccurs.")
      case (Never______, Implicit__, max, ___) => separatedContentExactlyN(max)
      case (Never______, ock /****/ , ___, __2) => SDE("separatorSuppressionPolicy='never' not allowed in combination with occursCountKind='" + ock + "'.")
      case (Trailing___, Implicit__, UNB, ___) if (!isLastRequiredElementOfSequence) => SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (Trailing___, Implicit__, UNB, min) => separatedContentUnbounded
      case (Trailing___, Implicit__, max, ___) => separatedContentAtMostN // FIXME: have to have all of them - not trailing position 
      case (TrailingStr, Implicit__, UNB, ___) if (!isLastRequiredElementOfSequence) => SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (TrailingStr, Implicit__, UNB, ___) => separatedContentUnboundedWithoutTrailingEmpties // we're depending on optionalEmptyPart failing on empty content.
      case (TrailingStr, Implicit__, max, ___) => separatedContentAtMostNWithoutTrailingEmpties
      case (Always_____, Implicit__, UNB, ___) => separatedContentUnbounded
      case (Always_____, Implicit__, max, ___) => separatedContentAtMostN
      case (Always_____, Parsed____, ___, __2) => separatedContentUnbounded
      case (Always_____, StopValue_, ___, __2) => separatedContentUnbounded
      case (policy /**/ , ock /****/ , max, __2) => SDE("separatorSuppressionPolicy='" + policy + "' not allowed with occursCountKind='" + ock + "'.")
    }
    res
  })
}

trait ElementDeclGrammarMixin { self: ElementBase with ElementDeclMixin =>

  override lazy val inputValueCalcOption = findPropertyOption("inputValueCalc")

}

trait GlobalElementDeclGrammarMixin
  extends LocalElementGrammarMixin // can be repeating if not root
  { self: GlobalElementDecl =>

  lazy val documentElement = Prod("documentElement", this, scalarDefaultable)

  lazy val document = Prod("document", this, {
    prims.UnicodeByteOrderMark(this) ~ documentElement
  })

}

/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

trait TermGrammarMixin { self: Term =>

  lazy val newVars = this.annotationObjs.filter { st =>
    st.isInstanceOf[DFDLNewVariableInstance]
  }.asInstanceOf[Seq[DFDLNewVariableInstance]]

  lazy val newVarStarts = newVars.map { _.gram }
  lazy val newVarEnds = newVars.map { _.endGram }

  lazy val dfdlScopeBegin = Prod("dfdlScopeBegin", this, newVarStarts.length > 0,
    newVarStarts.fold(EmptyGram) { _ ~ _ })

  lazy val dfdlScopeEnd = Prod("dfdlScopeEnd", this, newVarEnds.length > 0,
    newVarEnds.fold(EmptyGram) { _ ~ _ })

  def termContentBody: Prod

  // I am not sure we need to distinguish these two. 
  lazy val asTermInSequence = termContentBody
  lazy val asTermInChoice = termContentBody

  def separatedForPosition(body: => Gram) = {
    if (!isRepresented) body // no separators for things that have no representation in the data stream
    else {
      val res = prefixSep ~ infixSepRule ~ body ~ postfixSep
      res
    }
  }

  lazy val Some(es) = {
    //
    // Not sure how to assert this,
    // but an invariant we're assuming here is that we are NOT the 
    // root element, which has no enclosing sequence at all.
    //
    // The grammar rules shouldn't be asking for separated stuff
    // in that situation, so we shouldn't be here.
    //
    // TODO: FIXME:
    // Also note: we can get away with just looking upward for nearest enclosing
    // sequence because we have restrictions on what can be inside a choice,
    // and we disallow delimiters on choices. If one allows delimiters on 
    // choices... consider
    // <sequence dfdl:separator=",">
    //   <choice dfdl:initiator="[", terminator="]">
    //     <element ref="foo" maxOccurs="20"/>
    //     ...
    // In this case, what separates the multiple occurrances of foo? I claim 
    // they are comma separated.
    // But data could be like this 'a, b, c,[foo1,foo2,foo3],d,e,f'
    //
    // Not unreasonable, but just too much complexity. Postpone until later.

    //
    // TODO: fix this when those restrictions are lifted.
    //
    subset(hasES, "(Current restriction) There must be an enclosing sequence.")
    nearestEnclosingSequence
  }

  def hasES = nearestEnclosingSequence != None
  def ignoreES = inChoiceBeforeNearestEnclosingSequence == true

  lazy val staticSeparator = Prod("staticSeparator", this, !ignoreES && hasES && es.separator.isConstant,
    prims.StaticSeparator(es, self))

  lazy val dynamicSeparator = Prod("dynamicSeparator", this, !ignoreES && hasES && !es.separator.isConstant,
    prims.DynamicSeparator(es, self))

  lazy val sepRule = staticSeparator | dynamicSeparator

  lazy val prefixSep = Prod("prefixSep", this,
    {
      val res = !ignoreES && hasES && es.hasPrefixSep
      res
    },
    sepRule)

  lazy val postfixSep = Prod("postfixSep", this, !ignoreES && hasES && es.hasPostfixSep, sepRule)
  lazy val infixSep = Prod("infixSep", this, !ignoreES && hasES && es.hasInfixSep, sepRule)

  lazy val isStaticallyFirst = {
    es.hasInfixSep &&
      this.positionInNearestEnclosingSequence == 1 &&
      isScalar &&
      !hasPriorRequiredSiblings
  }

  lazy val infixSepRule = Prod("infixSepRule", this,
    !ignoreES && hasES && es.hasInfixSep, {
      if (isStaticallyFirst) prims.Nada(this) // we're first, no infix sep.
      else if (hasPriorRequiredSiblings) infixSep // always in this case
      else if (positionInNearestEnclosingSequence > 1 || !isScalar) {
        // runtime check for group pos such that we need a separator.
        // Note that GroupPosGreaterThan(N,..) sets discriminator, so if it is true, and infixSep is not found, it won't
        // backtrack and try nothing. Only if GroupPos is not greater than N will it backtrack.
        // TODO: adding ChildPosGreaterThan and ArrayPosGreaterThan fixes bug with xs:choice and array tests--check for other cases
        (prims.ArrayPosGreaterThan(1, self) ~ infixSep) |
          ((prims.GroupPosGreaterThan(1, self) ~ infixSep) | prims.Nada(this))
      } else Assert.invariantFailed("infixSepRule didn't understand what to lay down as grammar for this situation: " + this)
    })

}

trait HasStatementsGrammarMixin { self: Term with DFDLStatementMixin =>

  final lazy val statementGrams = statements.map { _.gram }
  // TODO: statements (but specifically not newVariableInstance) can appear on simple type definitions as well as terms.

  lazy val dfdlStatementEvaluations = Prod("dfdlStatementEvaluations", this, statementGrams.length > 0,
    statementGrams.fold(EmptyGram) { _ ~ _ })
}

trait ModelGroupGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin
  with GroupCommonAGMixin { self: ModelGroup =>

  lazy val groupLeftFraming = Prod("groupLeftFraming", this, leadingSkipRegion ~ alignmentFill ~ initiatorRegion)
  lazy val groupRightFraming = Prod("groupRightFraming", this, terminatorRegion ~ trailingSkipRegion)

  // I believe we can have the same grammar rules whether we're directly inside a complex type, or
  // we're nested inside another group as a term.
  lazy val asChildOfComplexType = termContentBody

  lazy val modelGroupSyntax = Prod("modelGroupSyntax", this, dfdlStatementEvaluations ~ groupLeftFraming ~ groupContent ~ groupRightFraming)

  lazy val termContentBody = Prod("termContentBody", this, separatedForPosition(modelGroupSyntax))

  def mt = EmptyGram.asInstanceOf[Gram] // cast trick to shut up foldLeft compile errors below

  def groupContent: Prod
}

trait ChoiceGrammarMixin { self: Choice =>

  lazy val groupContent = Prod("choiceContent", this, alternatives.foldRight(mt)(folder))

  def folder(p: Gram, q: Gram): Gram = p | q

  lazy val alternatives = groupMembers.map { _.asTermInChoice }
}

trait SequenceGrammarMixin { self: Sequence =>

  lazy val groupContent = Prod("sequenceContent", this, prims.StartSequence(this) ~ terms.foldRight(mt)(folder) ~ prims.EndSequence(this))

  def folder(p: Gram, q: Gram): Gram = p ~ q

  lazy val terms = groupMembers.map { _.asTermInSequence }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)

  lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)

  lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  lazy val hasSeparator = separator.isKnownNonEmpty

  // note use of pass by value. We don't want to even need the SeparatorPosition property unless there is a separator.
  def sepExpr(pos: => SeparatorPosition): Boolean = {
    if (hasSeparator) if (separatorPosition eq pos) true else false
    else false
  }
}

trait GroupRefGrammarMixin { self: GroupRef =>

  def termContentBody = self.group.termContentBody

}

/////////////////////////////////////////////////////////////////
// Types System
/////////////////////////////////////////////////////////////////

trait ComplexTypeBaseGrammarMixin { self: ComplexTypeBase =>
  lazy val startChildren = prims.StartChildren(this, true)
  lazy val endChildren = prims.EndChildren(this, true)

  lazy val mainGrammar = Prod("mainGrammar", self.element, startChildren ~ modelGroup.group.asChildOfComplexType ~ endChildren)

}
