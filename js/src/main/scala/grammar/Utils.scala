package grammar

import org.scalajs.jquery.{JQueryStatic, JQueryEventObject, JQuery}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

/**
 * Created by Mikael on 10.08.2015.
 */
trait JQueryExtended extends JQuery {
  def width(value: String): JQuery = js.native
  def alert(): JQuery = js.native

  @JSName("val") def value(e: js.Any): JQuery = js.native
  def html(e: js.Any): JQuery = js.native

  def autocomplete(e: js.Any, f: js.Any = null): JQuery = js.native
}

trait JQueryEventObjectExtended extends JQueryEventObject {
  val keyCode: Int = js.native
}

trait JQueryStaticExtended extends js.Any {
  val ui: JQueryStaticExtendedUi = js.native
}
trait JQueryStaticExtendedUi extends js.Any {
  val autocomplete: JQueryStaticExtendedUiAutoComplete = js.native
}
trait JQueryStaticExtendedUiAutoComplete extends js.Any {
  def filter(a: js.Array[String], part: String): js.Array[String] = js.native
}


object JQueryExtended {
  implicit def toJQueryStaticExtended(t: JQueryStatic): JQueryStaticExtended = t.asInstanceOf[JQueryStaticExtended]
  @inline implicit def toJQueryExtended(t: JQuery): JQueryExtended = t.asInstanceOf[JQueryExtended]
  @inline implicit def dynamicToBoolean(d: js.Dynamic): Boolean = d.asInstanceOf[Boolean]
  //@inline implicit def dynamicToString(d: js.Dynamic): String = d.asInstanceOf[String]
  //@inline implicit def dynamicToHandlerDataArgument(d: js.Dynamic): HandlerDataArgument = d.asInstanceOf[HandlerDataArgument]

  @inline implicit def toJQueryEventObjectExtended(t: JQueryEventObject) = t.asInstanceOf[JQueryEventObjectExtended]

  implicit class ComparisonOp(d: js.Dynamic) {
    def ==(other: String) = d.asInstanceOf[String] == other
  }
}
