/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.dfa

import scala.util.parsing.input.Reader
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.processors.WSP
import edu.illinois.ncsa.daffodil.processors.NL
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.DelimBase
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

/**
 * This base class handles the connections from state to state (which form a directed graph)
 * through use of call-by-name here and lazy vals within.  This allows us to define the
 * state-to-state connections functionally.
 *
 * This object itself will exist and be a member of the states ArrayBuffer as well as the other
 * states before any access to the lazy val members.
 */
abstract class State(states: => ArrayBuffer[State]) extends Serializable {

  def stateNum: Int
  def stateName: String
  def rules: ArrayBuffer[Rule]

  /**
   * A state executes by evaluating guarded actions.
   * if rules(n).test() evaluates to true, then take
   * action by rules(n).act().
   */
  def run(actNum: Int, r: Registers): Either[DFAStatus, Int] = {
    var actionNum = actNum //0
    while (actionNum < rules.length) {
      val rule = rules(actionNum)
      val useThisRule = rule.test(r)
      if (useThisRule) {
        val res = rule.act(r)
        res match {
          case Right(nextStateNum) => return Right(nextStateNum)
          case Left(status) => return Left(new DFAStatus(stateNum, actionNum, status))
        }
      }
      actionNum = actionNum + 1
    }
    Left(new DFAStatus(stateNum, actionNum, StateKind.Failed))
  }

  def findState(name: String): Int = {
    states.find(st => st.stateName == name).get.stateNum
  }
  // this is just one way to plumb the things together.
  // it depends on every state having a unique name
  //
  lazy val EECState = findState("EECState")
  lazy val ECState = findState("ECState")
  lazy val StartState = findState("StartState")
  lazy val PTERMState = findState("PTERM0")

  def printStr(): String = states.mkString
  override def toString(): String = stateName + "_" + stateNum

  def couldBeFirstChar(charIn: Char, delims: Seq[DFADelimiter]): Boolean = {
    // looking at data0
    delims.foreach(d => if (couldBeFirstChar(charIn, d)) return true)
    false
  }

  /**
   * Determines if the supplied character (charIn) could be
   * the start of this delimiter.
   *
   * This should only ever be called from DFAField.
   *
   * In the case of WSPStar, if charIn is not a whitespace then
   * we must compare charIn to the next character in the delimiter
   * if it's present.
   *
   */
  private def couldBeFirstChar(charIn: Char, d: DFADelimiter): Boolean = {
    val states = d.states
    val pTerm0 = states(0)
    val res =
      if (pTerm0.isInstanceOf[WSPStarState]) {
        val wspStar = pTerm0.asInstanceOf[WSPStarState]
        if (wspStar.checkMatch(charIn)) { true } // Was a space 
        else if (wspStar.nextState == DFA.FinalState) {
          // WSP* is not allowed to appear by itself as a terminator
          // or separator.
          //
          Assert.impossibleCase
        } else {
          // Wasn't a space, this is OK because it can be optional. 
          // The character must match the next state in order for it to
          // be the start of a delimiter.
          //
          val pTerm1 = states(1).asInstanceOf[DelimStateBase]
          pTerm1.checkMatch(charIn)
        }
      } else {
        pTerm0.asInstanceOf[DelimStateBase].checkMatch(charIn)
      }
    res
  }
}

// The delimiter compiler still has to select and instantiate all the appropriate states
// Note that there is no reason why a state has to have a fixed static set of rules.
// The compiler could create rule objects and add them to the rules. This is what
// we would expect when compiling a delimiter, which can mean any terminating markup
// each of which is specified as a list of individual delimiters. Each of those
// can have char class entities like WSP+ in it, and so on. 
// 

/**
 * Ambiguity Truth Table
 * 0 means not same, 1 means same
 * ------------------------------------
 * EC		EEC		PTERM	Ambiguous?
 * 0		1		1		Y
 * 1		0		1		Y
 * 1		1		0		Y
 * 1		1		1		Y
 */
class StartStateUnambiguousEscapeChar(states: => ArrayBuffer[State], EEC: Maybe[Char], EC: Maybe[Char], val stateNum: Int)
  extends State(states) {

  val stateName: String = "StartState"
  val rules = ArrayBuffer(
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { EEC.isDefined && EC.isDefined && r.data0 == EEC.get } } { (r: Registers) => Right(EECState) },
    Rule { (r: Registers) => EC.isDefined && r.data0 == EC.get } { (r: Registers) => Right(ECState) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => Right(DFA.EndOfData) },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    })

}

class StartState(states: => ArrayBuffer[State], val stateNum: Int)
  extends State(states) {

  val stateName: String = "StartState"
  val rules = ArrayBuffer(
    Rule { (r: Registers) =>
      couldBeFirstChar(r.data0, r.delimiters)
    } { (r: Registers) =>
      Left(StateKind.Paused)
    },
    Rule { (r: Registers) =>
      {
        r.data0 == DFA.EndOfDataChar
      }
    } { (r: Registers) =>
      Right(DFA.EndOfData)
    },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    })

}

class StartStatePadding(states: => ArrayBuffer[State], val padChar: Char)
  extends State(states) {

  val stateName = "StartState"
  val stateNum: Int = 0
  val rules = ArrayBuffer(
    Rule { (r: Registers) => { r.data0 == padChar } } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    },
    Rule { (r: Registers) => true } { (r: Registers) => { Right(DFA.FinalState) } })
}

/**
 * StartState for Field portion of EscapeBlock.
 * Here compiledDelims should only contain the endBlock DFADelimiter.
 */
class StartStateEscapeBlock(states: => ArrayBuffer[State], val blockEnd: DFADelimiter, val EEC: Maybe[Char], val stateNum: Int)
  extends State(states) {
  val stateName: String = "StartState"

  val rules = ArrayBuffer(
    Rule { (r: Registers) => couldBeFirstChar(r.data0, Seq(blockEnd)) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { EEC.isDefined && (r.data0 == EEC.get) } } { (r: Registers) => Right(EECState) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => Right(DFA.EndOfData) },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance
        Right(StartState)
      }
    })

}

class StartStateEscapeChar(states: => ArrayBuffer[State], val EEC: Maybe[Char], val EC: Maybe[Char], val stateNum: Int)
  extends State(states) {

  val stateName: String = "StartState"

  val rules_NO_EEC_BUT_EC_TERM_SAME = ArrayBuffer(
    Rule { (r: Registers) => (r.data0 == EC.get) && couldBeFirstChar(r.data1, r.delimiters) } { (r: Registers) => Right(ECState) },
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { r.data0 == EC.get } } { (r: Registers) => Right(ECState) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => { Right(DFA.EndOfData) } },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance
        Right(StartState)
      }
    })

  val rules_EEC_EC_SAME_NOT_TERM = ArrayBuffer(
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get && r.data1 == EC.get } } { (r: Registers) => Right(EECState) },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get && r.data1 != EC.get } } { (r: Registers) => Right(ECState) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => Right(DFA.EndOfData) },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance
        Right(StartState)
      }
    })

  val rules_EEC_TERM_SAME_NOT_EC = ArrayBuffer(
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => { Left(StateKind.Paused) } },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get && r.data1 == EC.get } } { (r: Registers) => Right(EECState) },
    Rule { (r: Registers) => { r.data0 == EC.get } } { (r: Registers) => Right(ECState) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => Right(DFA.EndOfData) },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance
        Right(StartState)
      }
    })

  val rules_EC_TERM_SAME_NOT_EEC = ArrayBuffer(
    Rule { (r: Registers) => { r.data0 == EC.get && r.data1 == EC.get } } { (r: Registers) =>
      {
        // EC followed by EC, first EC gets dropped
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance
        r.advance
        Right(StartState)
      }
    },
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get && r.data1 == EC.get } } { (r: Registers) => Right(EECState) },
    Rule { (r: Registers) => { r.data0 == EC.get } } { (r: Registers) => Right(ECState) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => Right(DFA.EndOfData) },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance
        Right(StartState)
      }
    })

  val rules_EC_EEC_TERM_SAME = ArrayBuffer(
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get && r.data1 == EEC.get } } {
      (r: Registers) => Right(EECState)
    },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get && r.data1 != DFA.EndOfDataChar } } {
      (r: Registers) =>
        {
          // EEC followed by something not a PTERM[2]
          r.dropChar(r.data0)
          r.appendToField(r.data1)
          r.advance
          r.advance
          Right(StartState)
        }
    },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get && r.data1 == DFA.EndOfDataChar } } {
      (r: Registers) => Right(ECState)
    },
    Rule { (r: Registers) => r.data0 == DFA.EndOfDataChar } { (r: Registers) => { Right(DFA.EndOfData) } },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance
        Right(StartState)
      }
    })

  val rules_Unambiguous = ArrayBuffer(
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { EEC.isDefined && r.data0 == EEC.get } } { (r: Registers) => Right(EECState) },
    Rule { (r: Registers) => { EC.isDefined && r.data0 == EC.get } } { (r: Registers) => Right(ECState) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => Right(DFA.EndOfData) },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    })

  val rules_NoEscaping = ArrayBuffer(
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } { (r: Registers) => Left(StateKind.Paused) },
    Rule { (r: Registers) => { r.data0 == DFA.EndOfDataChar } } { (r: Registers) => Right(DFA.EndOfData) },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    })

  /**
   * A state executes by evaluating guarded actions.
   * if rules(n).test() evaluates to true, then take
   * action by rules(n).act().
   */
  override def run(actNum: Int, r: Registers): Either[DFAStatus, Int] = {
    var actionNum = actNum //0
    val rules = getRules(r.delimiters)
    while (actionNum < rules.length) {
      if (rules(actionNum).test(r)) {
        val res = rules(actionNum).act(r)
        res match {
          case Right(nextStateNum) => return Right(nextStateNum)
          case Left(status) => return Left(new DFAStatus(stateNum, actionNum, status))
        }
      }
      actionNum = actionNum + 1
    }
    Left(new DFAStatus(stateNum, actionNum, StateKind.Failed))
  }

  /**
   * Determines what rules to execute based on combinations of EC/EEC
   */
  def getRules(delims: Seq[DFADelimiter]): ArrayBuffer[Rule] = {

    val result = {
      if (!EEC.isDefined) {
        if (!EC.isDefined) {
          // No Escaping
          rules_NoEscaping
        } else {
          if (couldBeFirstChar(EC.get, delims)) {
            // EC == PTERM0
            rules_NO_EEC_BUT_EC_TERM_SAME
          } else {
            // Unambiguous
            rules_Unambiguous
          }
        }
      } else if (EEC.isDefined && EC.isDefined) {
        val escEsc = EEC.get
        if (!couldBeFirstChar(escEsc, delims) && !couldBeFirstChar(EC.get, delims) && escEsc != EC.get) {
          // EC != EEC != PTERM0
          rules_Unambiguous
        } else if (EC.get == escEsc && couldBeFirstChar(escEsc, delims)) {
          // EC == EEC == PTERM0
          rules_EC_EEC_TERM_SAME
        } else if (couldBeFirstChar(escEsc, delims) && !couldBeFirstChar(EC.get, delims)) {
          // (EEC == PTERM0) != EC
          rules_EEC_TERM_SAME_NOT_EC
        } else if (!couldBeFirstChar(escEsc, delims) && couldBeFirstChar(EC.get, delims)) {
          // (EC == PTERM0) != EEC
          rules_EC_TERM_SAME_NOT_EEC
        } else if (EC.get == escEsc && !couldBeFirstChar(escEsc, delims)) {
          // (EC == EEC) != PTERM0
          rules_EEC_EC_SAME_NOT_TERM
        } else throw new Exception("Unexpected case.")
      } else throw new Exception("Unexpected case.")
    }
    result
  }

  var rules: ArrayBuffer[Rule] = ArrayBuffer.empty

}

class ECState(states: => ArrayBuffer[State], val EC: Maybe[Char], val stateNum: Int)
  extends State(states) {

  val stateName = "ECState"
  val rules = ArrayBuffer(
    // ECState, means that data0 is EC
    //
    Rule { (r: Registers) => couldBeFirstChar(r.data1, r.delimiters) } { (r: Registers) =>
      {
        // constituent character
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        Right(StartState)
      }
    },
    Rule { (r: Registers) => { r.data1 == EC.get } } { (r: Registers) =>
      {
        // next char is also EC, drop this EC
        // but do not treat next EC as char
        r.dropChar(r.data0)
        r.advance()
        Right(StartState)
      }
    },
    Rule { (r: Registers) => { r.data1 == DFA.EndOfDataChar } } { (r: Registers) =>
      {
        r.dropChar(r.data0)
        Right(DFA.EndOfData)
      }
    },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        // constituent character
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        Right(StartState)
      }
    })
}

class EECState(states: => ArrayBuffer[State], val EEC: Maybe[Char], val EC: Maybe[Char], val stateNum: Int)
  extends State(states) {

  val stateName = "EECState"
  val rules = ArrayBuffer(
    // Because this is about EC and EEC we can 
    // just write these rules down statically. 
    // The real delim compiler would generate a flock of rules
    // based on what the actual delimiters are.
    //
    // We've already encountered EEC as data0 here
    //
    Rule { (r: Registers) => couldBeFirstChar(r.data0, r.delimiters) } {
      (r: Registers) => Left(StateKind.Paused) //PTERMState
    },
    Rule { (r: Registers) => { EC.isDefined && r.data1 == EC.get } } { (r: Registers) =>
      {
        // Drop EEC aka data0
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        Right(StartState)
      }
    },
    Rule { (r: Registers) => { r.data1 == DFA.EndOfDataChar } } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        Right(DFA.EndOfData)
      }
    },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        // Not followed by an EC, therefore
        // it is an EEC and should remain in field.
        //
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    })
}

class EECStateBlock(states: => ArrayBuffer[State], blockEnd: DFADelimiter, val EEC: Maybe[Char], val stateNum: Int)
  extends State(states) {

  val stateName = "EECState"
  val rules = ArrayBuffer(
    // Because this is about EC and EEC we can 
    // just write these rules down statically. 
    // The real delim compiler would generate a flock of rules
    // based on what the actual delimiters are.
    //
    // We've already encountered EEC as data0 here
    //
    Rule { (r: Registers) => couldBeFirstChar(r.data0, Seq(blockEnd)) } {
      (r: Registers) => Left(StateKind.Paused) //PTERMState
    },
    Rule { (r: Registers) => couldBeFirstChar(r.data1, Seq(blockEnd)) } { (r: Registers) =>
      {
        // EEC followed by possible blockEnd
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance
        r.advance
        Right(StartState)
      }
    },
    Rule { (r: Registers) => { r.data1 == EEC.get } } { (r: Registers) =>
      {
        // EEC followed by EEC, stays in field
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    },
    Rule { (r: Registers) => { r.data1 == DFA.EndOfDataChar } } { (r: Registers) =>
      {
        r.appendToField(r.data0)
        Right(DFA.EndOfData)
      }
    },
    Rule { (r: Registers) => true } { (r: Registers) =>
      {
        // EEC followed by normal character
        r.appendToField(r.data0)
        r.advance()
        Right(StartState)
      }
    })
}

abstract class DelimStateBase(states: => ArrayBuffer[State])
  extends State(states) {
  var stateName: String = null
  def setStateName(name: String) = stateName = name
  def rules: ArrayBuffer[Rule]

  def nextState: Int

  def checkMatch(charIn: Char): Boolean

  override def toString(): String = {
    "<" + stateName + "_" + stateNum + "/>"
  }
}

class CharState(states: => ArrayBuffer[State], char: Char, val nextState: Int, val stateNum: Int)
  extends DelimStateBase(states) {

  stateName = "CharState(" + char + ")"
  val rulesToThisState = ArrayBuffer(
    Rule { (r: Registers) => { r.data0 == char } } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(stateNum)
      }
    })

  val rules = ArrayBuffer(
    Rule { (r: Registers) => { r.data0 == char } } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(nextState)
      }
    })

  def checkMatch(charIn: Char): Boolean = char == charIn
}

abstract class WSPBase(states: => ArrayBuffer[State])
  extends DelimStateBase(states) with WSP {

  def checkMatch(charIn: Char): Boolean = {
    val result = charIn match {
      case CTRL0 | CTRL1 | CTRL2 | CTRL3 | CTRL4 => true
      case SPACE | NEL | NBSP | OGHAM | MONG => true
      case SP0 | SP1 | SP2 | SP3 | SP4 | SP5 | SP6 | SP7 | SP8 | SP9 | SP10 => true
      case LSP | PSP | NARROW | MED | IDE => true
      case _ => false
    }
    result
  }
}

class WSPState(states: => ArrayBuffer[State], val nextState: Int, val stateNum: Int)
  extends WSPBase(states) {

  stateName = "WSPState"
  val rulesToThisState = ArrayBuffer(
    Rule { (r: Registers) => checkMatch(r.data0) } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(stateNum)
      }
    })

  val rules = ArrayBuffer(
    Rule { (r: Registers) => checkMatch(r.data0) } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(nextState)
      }
    })
}

abstract class WSPRepeats(states: => ArrayBuffer[State])
  extends WSPBase(states) {

}

class WSPPlusState(states: => ArrayBuffer[State], val nextState: Int, val stateNum: Int)
  extends WSPRepeats(states) {

  stateName = "WSPPlusState"
  val rulesToThisState = ArrayBuffer(
    Rule { (r: Registers) => checkMatch(r.data0) } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(stateNum) // This state
      }
    })

  val rules = ArrayBuffer(
    Rule { (r: Registers) => checkMatch(r.data0) } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        r.matchedAtLeastOnce = true
        Right(stateNum) // This state
      }
    },
    Rule { (r: Registers) => r.matchedAtLeastOnce } { (r: Registers) =>
      r.matchedAtLeastOnce = false
      Right(nextState)
    })
}

class WSPStarState(states: => ArrayBuffer[State], val nextState: Int, val stateNum: Int)
  extends WSPRepeats(states) {

  stateName = "WSPStarState"

  val rules = ArrayBuffer(
    Rule { (r: Registers) => checkMatch(r.data0) } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(stateNum) // This state
      }
    },
    Rule { (r: Registers) => true } {
      (r: Registers) => Right(nextState)
    })
  var rulesToNextStateMinusUs: ArrayBuffer[Rule] = ArrayBuffer.empty
}

abstract class NLBase(states: => ArrayBuffer[State])
  extends DelimStateBase(states) with NL {

  def isNLNotCR(charIn: Char): Boolean = {
    charIn match {
      case LF | NEL | LS => true
      case _ => false
    }
  }

  def isCR(charIn: Char): Boolean = {
    charIn match {
      case CR => true
      case _ => false
    }
  }

  def isLF(charIn: Char): Boolean = {
    charIn match {
      case LF => true
      case _ => false
    }
  }
}

class NLState(states: => ArrayBuffer[State], val nextState: Int, val stateNum: Int)
  extends NLBase(states) {

  stateName = "NLState"

  val rules = ArrayBuffer(
    Rule { (r: Registers) => { isCR(r.data0) && isLF(r.data1) } } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.appendToDelim(r.data1)
        r.advance
        r.advance
        Right(nextState)
      }
    },
    Rule { (r: Registers) => { isCR(r.data0) && !isLF(r.data1) } } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(nextState)
      }
    },
    Rule { (r: Registers) => isNLNotCR(r.data0) } { (r: Registers) =>
      {
        r.appendToDelim(r.data0)
        r.advance
        Right(nextState)
      }
    })

  def checkMatch(charIn: Char): Boolean = isNLNotCR(charIn) || isCR(charIn)
}