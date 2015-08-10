package grammar

import org.scalajs.jquery.JQuery

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

object JQueryExtended {
  implicit def toJQueryExtended(t: JQuery): JQueryExtended = t.asInstanceOf[JQueryExtended]
  implicit def dynamicToBoolean(d: js.Dynamic): Boolean = d.asInstanceOf[Boolean]
  implicit def dynamicToString(d: js.Dynamic): String = d.asInstanceOf[String]
  implicit def dynamicToHandlerDataArgument(d: js.Dynamic): HandlerDataArgument = d.asInstanceOf[HandlerDataArgument]

  implicit class ComparisonOp(d: js.Dynamic) {
    def ==(other: String) = d.asInstanceOf[String] == other
  }
}
