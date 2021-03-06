package edu.illinois.ncsa.daffodil.processors
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.NamedGram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._

object StmtEval {
  def apply(context: ElementBase, eGram: Gram) = {
    // mandatory little optimization here. If there are no statements (most common case), then let's 
    // shortcut and just use the guts parser.
    val hasStatements = context.notNewVariableInstanceStatements.size > 0
    if (hasStatements)
      new StmtEval(context, eGram)
    else
      eGram
  }
}

class StmtEval private (context: ElementBase, eGram: Gram) // private to force use of the object as factory
  extends NamedGram(context) {

  // The order of things matters in some cases, so to be consistent we'll always use the 
  // same order even when it doesn't matter

  // The order of evaluation of statements is:
  // - pattern discriminators
  // - pattern asserts
  // - the parsing of the element itself
  // - setVariables
  // - test discriminators (must be attempted even if the parsing of element or setVariable statements fail)
  // - test asserts

  // requiredEvaluations(patDiscrim, patAssert, eGram, setVar, testDiscrim, testAssert)
  // Note: above not needed as these are ALWAYS evaluated below.

  val patDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val patAssert = context.assertStatements.filter(_.testKind == TestKind.Pattern).map(_.gram)
  val setVar = context.setVariableStatements.map(_.gram)
  val testDiscrim = context.discriminatorStatements.filter(_.testKind == TestKind.Expression).map(_.gram)
  val testAssert = context.assertStatements.filter(_.testKind == TestKind.Expression).map(_.gram)

  val eParser = eGram.parser

  def parser: Parser = new StatementElementParser(context)

  class StatementElementParser(context: ElementBase) extends PrimParser(this, context) {

    Assert.invariant(context.notNewVariableInstanceStatements.size > 0) // if this fails, you probably allocated a StmtEval with new rather than using the StmtEval object.
    Assert.invariant(testDiscrim.size <= 1)
    Assert.invariant(patDiscrim.size <= 1)

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<StmtEval>" +
          patDiscrim.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          patAssert.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          eParser.toBriefXML(depthLimit - 1) +
          setVar.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          testDiscrim.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          testAssert.map { _.parser.toBriefXML(depthLimit - 1) }.mkString +
          "</StmtEval>"
    }

    def parse(pstate: PState): PState = {
      //Removed checks now done at compilation

      var afterPatDisc = pstate //we're first so don't do .withPos(pstate.bitPos, pstate.charPos)
      patDiscrim.map(_.parser).foreach(d => {
        afterPatDisc = d.parse1(afterPatDisc, context)
        // Pattern fails at the start of the Element
        if (afterPatDisc.status != Success) { return afterPatDisc }
      })

      // now here we backup and run the pattern Asserts 
      // against the data at the start of the element's representation again.
      var afterPatAssrt = afterPatDisc.withPos(pstate.bitPos, pstate.charPos, pstate.inStream.reader)
      patAssert.map(_.parser).foreach(d => {
        afterPatAssrt = d.parse1(afterPatAssrt, context)
        // Pattern fails at the start of the Element
        if (afterPatAssrt.status != Success) { return afterPatAssrt }
      })

      // backup again. If all pattern discriminators and/or asserts
      // have passed, now we parse the element. But we backup
      // as if the pattern matching had not advanced the state.
      val beforeEState = afterPatAssrt.withPos(pstate.bitPos, pstate.charPos, pstate.inStream.reader)
      val postEState = eParser.parse1(beforeEState, context)

      var someSetVarFailed: Option[PState] = None

      var afterSetVar = postEState
      if (postEState.status == Success) {
        setVar.map(_.parser).foreach(d => {
          val afterOneSetVar = d.parse1(afterSetVar, context)
          if (afterOneSetVar.status == Success) {
            afterSetVar = afterOneSetVar
          } else {
            // a setVariable statement failed. But we want to continue to try 
            // more of the setVariable statements, as they may be necessary
            // to evaluate the test discriminator below, and some might 
            // be successful even if one fails, allowing the discriminator to be true.
            //
            // So it's a bit odd, but we're going to just keep parsing using this
            // failed state as the input to the next setVariable parse step.
            someSetVarFailed = Some(afterOneSetVar)
            afterSetVar = afterOneSetVar
          }
        })
      }

      var afterTestDisc = afterSetVar
      testDiscrim.map(_.parser).foreach(d => {
        afterTestDisc = d.parse1(afterTestDisc, context)
        // Tests fail at the end of the Element
        if (afterTestDisc.status != Success) { return afterTestDisc }
      })

      // 
      // We're done with the discriminator, so now we revisit the set variable statements.
      // If a failure occurred there, then now we can fail out right here.
      // 
      someSetVarFailed.exists { return _ }

      // Element evaluation failed, return
      if (postEState.status != Success) { return postEState }

      var afterTestAssrt = afterTestDisc
      testAssert.map(_.parser).foreach(d => {
        afterTestAssrt = d.parse1(afterTestAssrt, context)
        // Tests fail at the end of the Element
        if (afterTestAssrt.status != Success) { return afterTestAssrt }
      })

      afterTestAssrt
    }
  }

  def unparser: Unparser = new Unparser(context) {
    def unparse(start: UState): UState = {
      // FIXME: setVariables have to execute. We don't do asserts and discriminators when unparsing however.
      val eUnParser = eGram.unparser
      val postEState = eUnParser.unparse(start)
      postEState
    }
  }

}
