package grammar

import org.scalajs.jquery.{JQueryEventObject, JQuery}

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
}

trait JQueryEventObjectExtended extends JQueryEventObject {
  val keyCode: Int = js.native
}

object JQueryExtended {
  @inline implicit def toJQueryExtended(t: JQuery): JQueryExtended = t.asInstanceOf[JQueryExtended]
  @inline implicit def dynamicToBoolean(d: js.Dynamic): Boolean = d.asInstanceOf[Boolean]
  //@inline implicit def dynamicToString(d: js.Dynamic): String = d.asInstanceOf[String]
  //@inline implicit def dynamicToHandlerDataArgument(d: js.Dynamic): HandlerDataArgument = d.asInstanceOf[HandlerDataArgument]

  @inline implicit def toJQueryEventObjectExtended(t: JQueryEventObject) = t.asInstanceOf[JQueryEventObjectExtended]

  implicit class ComparisonOp(d: js.Dynamic) {
    def ==(other: String) = d.asInstanceOf[String] == other
  }
}
