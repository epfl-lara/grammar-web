package grammar.adminmode

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/**
 * Created by Mikael on 13.08.2015.
 */

case class EditableFieldInput(onBlur: () => Unit, display: Boolean, value: String, onChange: String => Unit, multiline: Boolean) {
  import EditableFieldInput._
  def build = editableFieldInput(this)
  def buildWithRef(ref: String) = editableFieldInput.withRef(ref)(this)
  def build(modifiers: (editableFieldInput.type => editableFieldInput.type)) = modifiers(editableFieldInput)(this)
}
object EditableFieldInput {
  case class State(value: String, savedValue: String)

  case class Backend($: BackendScope[EditableFieldInput, State]) {
    def setContent(value: String) = {
      $.modState((s: State) => s.copy(value = value))

    }

    def onChange(e: ReactEventI) = {
      setContent(e.target.value)
      $.props.onChange(e.target.value)
    }

    def state = $.state

    def resetContent() = $.modState((s: State) => s.copy(value = s.savedValue))

    def saveAndGetContent() = {
      if ($.state.savedValue == $.state.value) {
        None
      } else {
        $.modState((s: State) => s.copy(savedValue = s.value))
        Some(state.value)
      }
    }
  }

  val editableFieldInput = ReactComponentB[EditableFieldInput]("input text for a field")
    .initialStateP((props: EditableFieldInput) => State(props.value, props.value))
    .backend(Backend)
    .render((P, S, B) => {
    (if (P.multiline) <.textarea else <.input)(^.`class` := "admin-editable editable-part",
      ^.`type` := "text",
      ^.onChange ==> ((e: ReactEventI) => {
        B.onChange(e); P.onChange(e.target.value)
      }),
      ^.onBlur --> P.onBlur(),
      ^.display := (if (P.display) "inline-block" else "none"),
      ^.value := S.value
    )
  }).build
}
