package grammar.adminmode

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, BackendScope}

/**
 * Created by Mikael on 13.08.2015.
 */
case class Button(text: String, onClick: () => Unit, selected: Boolean = false) {
  def build = Button.adminButton(this)
  def buildWithKey(key: String) = Button.adminButton.withKey(key)(this)
}
object Button {
  case class AdminButtonState(selected: Boolean = false)

  case class AdminButtonBackend($: BackendScope[Button, AdminButtonState]) {
    def select(b: Boolean = true) = $.setState(AdminButtonState(b))
  }

  val adminButton = ReactComponentB[Button]("Open reference grammar button")
    .initialStateP(P => AdminButtonState(P.selected))
    .backend(AdminButtonBackend)
    .render((P, S, B) => {
    <.input(
      ^.classSet1("admin-button",
        "admin-button-selected" -> P.selected),
      ^.`type` := "button",
      ^.value := P.text,
      ^.onClick --> {
        P.onClick()
      })
  }).build
}