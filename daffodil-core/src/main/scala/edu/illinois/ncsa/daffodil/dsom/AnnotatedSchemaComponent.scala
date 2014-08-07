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
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.Text

/**
 * Shared characteristics of any annotated schema component.
 * Not all components can carry DFDL annotations.
 */

abstract class AnnotatedSchemaComponent(xml: Node, sc: SchemaComponent)
  extends SchemaComponent(xml, sc)
  with AnnotatedMixin {

  requiredEvaluations({
    annotationObjs
    shortFormPropertiesCorrect
    nonDefaultPropertySources
    defaultPropertySources
  })

  //  /**
  //   * only used for debugging
  //   */
  //  override lazy val properties: PropMap =
  //    (nonDefaultPropertySources.flatMap { _.properties.toSeq } ++
  //      defaultPropertySources.flatMap { _.properties.toSeq }).toMap

  final lazy val shortFormPropertiesCorrect: Boolean = {
    // Check that any unprefixed properties are disjoint with ALL DFDL property names.
    // Warning otherwise
    // Insure that some prefix is bound to the dfdl namespace. Warn otherwise.
    // Warn if dfdl: is bound to something else than the DFDL namespace. 
    shortFormAnnotationsAreValid
  }

  /**
   * Since validation of extra attributes on XML Schema elements is
   * normally lax validation, we can't count on validation of DFDL schemas
   * to tell us whether short-form annotations are correct or not.
   *
   * So, we have to do this check ourselves.
   *
   * TBD: change properties code generator to output the various lists of
   * properties that we have to check against. (Might already be there...?)
   *
   */
  def shortFormAnnotationsAreValid: Boolean = true

  def nonDefaultPropertySources: Seq[ChainPropProvider]

  def defaultPropertySources: Seq[ChainPropProvider]

  lazy val nonDefaultFormatChain: ChainPropProvider = formatAnnotation.getFormatChain()
  lazy val defaultFormatChain: ChainPropProvider = {
    val res = schemaDocument.formatAnnotation.getFormatChain()
    res
  }

  private def findDefaultOrNonDefaultProperty(
    pname: String,
    sources: Seq[ChainPropProvider]): PropertyLookupResult = {
    val seq = sources.map { _.chainFindProperty(pname) }
    val optFound = seq.collectFirst { case found: Found => found }
    val result = optFound match {
      case Some(f @ Found(_, _, _)) => f
      case None => {
        // merge all the NotFound stuff.
        val nonDefaults = seq.flatMap {
          case NotFound(nd, d) => nd
          case _: Found => Assert.invariantFailed()
        }
        val defaults = seq.flatMap {
          case NotFound(nd, d) => d
          case _: Found => Assert.invariantFailed()
        }
        Assert.invariant(defaults.isEmpty)
        val nf = NotFound(nonDefaults, defaults)
        nf
      }
    }
    result
  }

  private def findNonDefaultProperty(pname: String): PropertyLookupResult = {
    val result = findDefaultOrNonDefaultProperty(pname, nonDefaultPropertySources)
    result match {
      case f: Found => f
      case NotFound(nd, d) =>
        Assert.invariant(d.isEmpty)
    }
    result
  }

  private def findDefaultProperty(pname: String): PropertyLookupResult = {
    val result = findDefaultOrNonDefaultProperty(pname, defaultPropertySources)
    val fixup = result match {
      case f: Found => f
      case NotFound(nd, d) =>
        Assert.invariant(d.isEmpty)
        NotFound(Seq(), nd) // we want the places we searched shown as default locations searched
    }
    fixup
  }

  override def findPropertyOption(pname: String): PropertyLookupResult = {
    ExecutionMode.requireCompilerMode
    // first try in regular properties
    val regularResult = findNonDefaultProperty(pname)
    regularResult match {
      case f: Found => f
      case NotFound(nonDefaultLocsTried1, defaultLocsTried1) => {
        Assert.invariant(defaultLocsTried1.isEmpty)
        val defaultResult = findDefaultProperty(pname)
        defaultResult match {
          case f: Found => f
          case NotFound(nonDefaultLocsTried2, defaultLocsTried2) => {
            Assert.invariant(nonDefaultLocsTried2.isEmpty)
            // did not find it at all. Return a NotFound with all the places we 
            // looked non-default and default.
            val nonDefaultPlaces = nonDefaultLocsTried1
            val defaultPlaces = defaultLocsTried2
            NotFound(nonDefaultPlaces, defaultPlaces)
          }
        }
      }
    }
  }
}

/**
 * Every component that can be annotated.
 * Review Note:
 * It's no longer clear that this separation is strictly speaking needed.
 * It's possible that this could be collapsed back into AnnotatedSchemaComponent
 * or made smaller anyway.
 *
 */
trait AnnotatedMixin
  extends CommonRuntimeValuedPropertiesMixin
  with EncodingMixin
  with EscapeSchemeRefMixin { self: AnnotatedSchemaComponent =>

  def xml: Node
  def prettyName: String
  def path: String

  /**
   * Anything annotated must be able to construct the
   * appropriate DFDLAnnotation object from the xml.
   */
  def annotationFactory(node: Node): DFDLAnnotation

  lazy val annotationNode = {
    val ann = xml \ "annotation"
    ann
  }

  /**
   * dais = Dfdl App Info nodeSeq
   */
  lazy val dais = {
    val ais = (annotationNode \ "appinfo")
    val dais = ais.filter { ai =>
      {
        ai.attribute("source") match {
          case None => false
          case Some(n) => {
            val str = n.text
            //
            // Keep in mind. As the DFDL standard evolves, and new versions
            // come out, this code may change to tolerate different source
            // attributes that call out distinct versions of the standard.
            //
            val officialAppinfoSourceAttribute = XMLUtils.dfdlAppinfoSource
            val hasRightSource = (str == officialAppinfoSourceAttribute)
            val isAcceptable = str.contains("ogf") && str.contains("dfdl")
            schemaDefinitionWarningWhen(!hasRightSource && isAcceptable,
              "The xs:appinfo source attribute value '%s' should be '%s'.", str, officialAppinfoSourceAttribute)
            (hasRightSource || isAcceptable)
          }
        }
      }
    }
    dais
  }

  /**
   * The DFDL annotations on the component, as objects
   * that are subtypes of DFDLAnnotation.
   */
  lazy val annotationObjs = annotationObjs_.value
  private val annotationObjs_ = LV('annotationObjs) {
    // println(dais)
    dais.flatMap { dai =>
      {
        val children = dai.child
        val res = children.filterNot { _.isInstanceOf[Text] }.map { child =>
          {
            annotationFactory(child)
          }
        }
        res
      }
    }
  }

  /**
   * Here we establish an invariant which is that every annotatable schema component has, definitely, has an
   * annotation object. It may have no properties on it, but it will be there. Hence, we can
   * delegate various property-related attribute calculations to it.
   *
   * To realize this, every concrete class must implement (or inherit) an implementation of
   * emptyFormatFactory, which constructs an empty format annotation,
   * and isMyFormatAnnotation which tests if an annotation is the corresponding kind.
   *
   * Given that, formatAnnotation then either finds the right annotation, or constructs one, but our invariant
   * is imposed. There *is* a formatAnnotation.
   */
  def emptyFormatFactory: DFDLFormatAnnotation
  def isMyFormatAnnotation(a: DFDLAnnotation): Boolean

  lazy val formatAnnotation = formatAnnotation_.value
  private val formatAnnotation_ = LV('formatAnnotation) {
    val format = annotationObjs.collect { case fa: DFDLFormatAnnotation if isMyFormatAnnotation(fa) => fa }
    val res = format match {
      case Seq() => emptyFormatFactory // does make things with the right namespace scopes attached!
      case Seq(fa) => fa
      case _ => schemaDefinitionError("Only one format annotation is allowed at each annotation point.")
    }
    res
  }

  lazy val justThisOneProperties = formatAnnotation.justThisOneProperties

}