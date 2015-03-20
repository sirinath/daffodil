package edu.illinois.ncsa.daffodil.xml

import scala.xml._
import spray.json._
import spray.json.DefaultJsonProtocol._
import edu.illinois.ncsa.daffodil.exceptions.Assert

trait ToJSON {

  /**
   * spray.json to map element-only (plus nil) XML to JSON
   *
   * The only attribute we consider is xsi:nil.
   */
  implicit object NodeFormat extends JsonFormat[Node] {
    def write(node: Node): JsValue = {
      node match {
        case Elem(pre, label, attrs, scope, c @ _*) => {
          val jsonnom = if (pre == null) label else pre + ":" + label
          c match {
            case Seq() => {
              attrs.get(XMLUtils.XSI_NAMESPACE, scope, "nil").map { _.text.toLowerCase } match {
                case Some("true") => JsObject(jsonnom -> JsNull)
                case None | Some(_) => JsObject(jsonnom -> JsArray())
              }
            }
            case Seq(n) => JsObject(jsonnom -> n.toJson)
            case cs => {
              val fields = cs.flatMap { _.toJson.asInstanceOf[JsObject].fields }
              val groups = fields.groupBy { case (n, v) => n }
              val arrayGroups = groups.map {
                case (n, Seq((_, one))) => (n, one)
                case (n, arrPairs) => {
                  Assert.invariant(arrPairs.length > 1)
                  val arr = arrPairs.collect { case (_, v) => v }
                  (n, JsArray(arr: _*))
                }
              }
              val fieldsMap = arrayGroups.toMap
              val obj = JsObject(jsonnom -> JsObject(fieldsMap))
              obj
            }
          }
        }
        case Text(s) => JsString(s)
        case _ if (node.child.count(_.isInstanceOf[Elem]) == 0) => {
          // all content nodes.
          JsString(node.text)
        }
        case ns: NodeSeq =>
          JsArray(node.child.map { write(_) }: _*)
      }
    }
    def read(jsValue: JsValue) = null // not implemented
  }
}

