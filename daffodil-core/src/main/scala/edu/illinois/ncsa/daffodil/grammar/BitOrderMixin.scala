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

package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.equality._

trait BitOrderMixin extends GrammarMixin { self: Term =>

  private val littleEndian = ByteOrder.LittleEndian.toString

  def checkBitOrderByteOrderCompatibility(bitOrd: BitOrder): BitOrder = {
    bitOrd match {
      case BitOrder.LeastSignificantBitFirst => {
        if (byteOrder.isConstant) {
          val literalByteOrder = byteOrder.constantAsString
          schemaDefinitionUnless(literalByteOrder =:= littleEndian,
            "Bit order 'leastSignificantBitFirst' requires byte order 'littleEndian' but was '%s'.", literalByteOrder)
        }
      }
      case BitOrder.MostSignificantBitFirst => {
        // ok with either big or little endian.
      }
    }
    bitOrd
  }

  final lazy val defaultBitOrder: BitOrder = {
    val bitOrd =
      if (DaffodilTunableParameters.requireBitOrderProperty) {
        checkBitOrderByteOrderCompatibility(bitOrder)
      } else {
        optionBitOrder.map { bitOrd =>
          // Only need to check compatibility if it is specified
          // because otherwise it is MostSignificantBitFirst, which can't conflict.
          checkBitOrderByteOrderCompatibility(bitOrd)
        }.getOrElse(BitOrder.MostSignificantBitFirst)
      }
    bitOrd
  }

  private lazy val enclosingBitOrder = enclosingTerm.map(_.defaultBitOrder)
  private lazy val priorSiblingBitOrder = priorSibling.map(_.defaultBitOrder)
  private lazy val bitOrderBefore = priorSiblingBitOrder.getOrElse(enclosingBitOrder.getOrElse(defaultBitOrder))

  protected final lazy val bitOrderChange = prod("bitOrderChange", bitOrderBefore != defaultBitOrder) { BitOrderChange(this) }
}

