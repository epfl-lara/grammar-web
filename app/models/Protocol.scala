package models

import play.api.libs.json._
import play.api.libs.iteratee._

object Protocol {
  case object Init
  case class InitSuccess(enum: Enumerator[JsValue])
  case class InitFailure(error: String)

  case class FromClient(event: JsValue)

  case object Quit
}
