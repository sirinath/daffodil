package edu.illinois.ncsa.daffodil.processors
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGException
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.ErrorAlreadyHandled
import edu.illinois.ncsa.daffodil.debugger.Debugger
import org.jdom.Namespace
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.compiler.ProcessorFactory

/**
 * Implementation mixin - provides simple helper methods
 */
trait WithDiagnosticsImpl extends WithDiagnostics {

  //  final lazy val hasDiagnostics = {
  //    getDiagnostics.size > 0
  //  }
}

/**
 * The very last aspects of compilation, and the start of the
 * back-end runtime.
 */
class DataProcessor(pf: ProcessorFactory, val rootElem: GlobalElementDecl)
  extends SchemaComponentBase(<dp/>, pf)
  with ImplementsThrowsSDE
  with DFDL.DataProcessor {

  def minMajorJVersion = 1
  def minMinorJVersion = 7
  def checkJavaVersion = {
    val jVersion = {
      try { System.getProperty("java.version") }
      catch {
        case se: SecurityException => this.SDE("Attempted to read property 'java.version' failed due to a SecurityException: \n%s".format(se.getMessage()))
        case _: Throwable => this.SDE("An invalid 'key' was passed to System.getProperty.")
      }
    }
    val javaVersion = """([0-9])\.([0-9])\.(.*)""".r
    jVersion match {
      case javaVersion(major, minor, x) => {

        if (major.toInt < minMajorJVersion) {
          this.SDE("You must run Java 7 (1.7) or higher. You are currently running %s".format(jVersion))
        }
        if (minor.toInt < minMinorJVersion) {
          this.SDE("You must run Java 7 (1.7) or higher. You are currently running %s".format(jVersion))
        }
      }
      case _ => {
        this.SDE("Failed to obtain the Java version.  You must run Java 7 (1.7) or higher.")
      }
    }
  }

  requiredEvaluations(
    parser,
    // force creation of the parser value so that all errors are issued
    // this is in case some compilation happens in the constructors of parsers.
    rootElem)

  Assert.usage(pf.canProceed)

  lazy val processorFactory = pf

  override lazy val fileName = processorFactory.fileName

  // just delegate to the PF. It has access to the SchemaSet.
  override def isError = pf.isError
  override def diagnostics = pf.diagnostics

  //
  // Last tidbits of compilation. Really this is just accessing the 
  // result of compilation
  // 
  lazy val parser = parser_.value
  private val parser_ = LV('parser) {
    ExecutionMode.usingCompilerMode {
      checkJavaVersion
      rootElem.document.parser
    }
  }

  lazy val unparser = unparser_.value
  private val unparser_ = LV('unparser) {
    ExecutionMode.usingCompilerMode {
      checkJavaVersion
      rootElem.document.unparser
    }
  }

  def save(output: DFDL.Output): Unit = {
    Assert.notYetImplemented()
  }

  /**
   * Here begins the parser runtime. Compiler-oriented mechanisms (OOLAG etc.) aren't used in the
   * runtime. Instead we deal with success and failure statuses.
   */
  def parse(input: DFDL.Input, lengthLimitInBits: Long = -1): DFDL.ParseResult = {
    Assert.usage(!this.isError)

    val initialState = PState.createInitialState(this.processorFactory.sset.schemaComponentRegistry,
      rootElem,
      input,
      bitOffset = 0,
      bitLengthLimit = lengthLimitInBits) // TODO also want to pass here the externally set variables, other flags/settings.
    try {
      Debugger.init(parser)
      parse(initialState)
    } finally {
      Debugger.fini(parser)
    }
  }

  def parse(initialState: PState) = {

    ExecutionMode.usingRuntimeMode {
      val pr = new ParseResult(this) {
        val p = parser
        val resultState = { // Not lazy. We want to parse right now.
          try {
            p.parse1(initialState, rootElem)
          } catch {
            // technically, runtime shouldn't throw. It's really too heavyweight a construct. And "failure" 
            // when parsing isn't exceptional, it's routine behavior. So ought not be implemented via an 
            // exception handling construct.
            //
            // But we might not catch everything inside...
            //
            case pe: ParseError => {
              // if we get one here, then someone threw instead of returning a status. 
              Assert.invariantFailed("ParseError caught. ParseErrors should be returned as failed status, not thrown. Fix please.")
            }
            case procErr: ProcessingError => {
              Assert.invariantFailed("got a processing error that was not a parse error. This is the parser!")
            }
            case sde: SchemaDefinitionError => {
              // A SDE was detected at runtime (perhaps due to a runtime-valued property like byteOrder or encoding)
              // These are fatal, and there's no notion of backtracking them, so they propagate to top level
              // here.
              initialState.failed(sde)
            }
            case rsde: RuntimeSchemaDefinitionError => {
              initialState.failed(rsde)
            }
            case e: ErrorAlreadyHandled => {
              initialState.failed(e.th)
              // Assert.invariantFailed("OOLAGException at runtime (should not happen). Caught at DataProcessor level: " + e)
            }
          }
        }
      }
      pr
    }
  }

  /**
   * Unparser runtime begins here. Compiler-oriented mechanisms (OOLAG etc.) aren't used in the
   * runtime. Instead we deal with success and failure statuses.
   */
  def unparse(output: DFDL.Output, infoset: scala.xml.Node): DFDL.UnparseResult = {
    Assert.usage(!this.isError)

    ExecutionMode.usingRuntimeMode {
      val jdomElem = XMLUtils.elem2Element(infoset)
      val jdomDoc = new org.jdom.Document(jdomElem)
      val initialState = UState.createInitialState(rootElem, output, jdomDoc) // also want to pass here the externally set variables, other flags/settings.

      val uRes = new UnparseResult(this) {
        val resultState = { // Not lazy. We want to unparse right now.

          try {
            unparser.unparse(initialState)
          } catch {
            // technically, runtime shouldn't throw. It's really too heavyweight a construct. And "failure" 
            // when parsing isn't exceptional, it's routine behavior. So ought not be implemented via an 
            // exception handling construct.
            //
            // But we might not catch everything inside...
            //
            case pe: UnparseError => {
              // if we get one here, then someone threw instead of returning a status. 
              Assert.invariantFailed("UnparseError caught. UnparseErrors should be returned as failed status, not thrown. Fix please.")
            }
            case procErr: ProcessingError => {
              Assert.invariantFailed("got a processing error that was not an unparse error. This is the unparser!")
            }
            case sde: SchemaDefinitionError => {
              // A SDE was detected at runtime (perhaps due to a runtime-valued property like byteOrder or encoding)
              // These are fatal, and there's no notion of backtracking them, so they propagate to top level here.
              initialState.failed(sde)
            }
            case e: OOLAGException => {
              Assert.invariantFailed("OOLAGExceptions like " + e.toString() + " are compiler stuff. This is runtime.")
            }
          }
        }
        //write unparsed result to outputStream
        resultState.outStream.write()
      }
      uRes
    }
  }
}

abstract class ParseResult(dp: DataProcessor)
  extends DFDL.ParseResult
  with WithDiagnosticsImpl {

  def resultState: PState

  lazy val result =
    if (resultState.status == Success) {
      if (resultAsJDOMDocument.hasRootElement()) XMLUtils.element2Elem(resultAsJDOMDocument.getRootElement())
      else <dafint:document xmlns:dafint={ XMLUtils.INT_NS }/>
    } else {
      Assert.abort(new IllegalStateException("There is no result. Should check by calling isError() first."))
    }

  lazy val resultAsJDOMDocument =
    if (resultState.status == Success) {
      val xmlClean = resultState.infoset.jdomElt match {
        case Some(e) => {
          XMLUtils.removeHiddenElements(e)
          XMLUtils.removeAttributesJDOM(e, Seq(Namespace.getNamespace(XMLUtils.INT_PREFIX, XMLUtils.INT_NS)))
          e.getDocument()
        }
        case None => Assert.impossibleCase() // Shouldn't happen, success means there IS a result.
      }
      xmlClean
    } else {
      Assert.abort(new IllegalStateException("There is no result. Should check by calling isError() first."))
    }
}

abstract class UnparseResult(dp: DataProcessor)
  extends DFDL.UnparseResult
  with WithDiagnosticsImpl {

  override def resultState: UState
}

