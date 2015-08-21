package grammar.adminmode

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, BackendScope}

/**
 * Created by Mikael on 13.08.2015.
 */
case class Button(text: String, onClick: () => Unit, selected: Boolean = false) {
  def build = Button.adminButton(this)
  def buildWithRef(ref: String) = Button.adminButton.withRef(ref)(this)
}
object Button {
  case class State(selected: Boolean = false)

  case class Backend($: BackendScope[Button, State]) {
    def select(b: Boolean = true) = $.setState(State(b))
  }

  val adminButton = ReactComponentB[Button]("Open reference grammar button")
    .initialStateP(P => State(P.selected))
    .backend(Backend)
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