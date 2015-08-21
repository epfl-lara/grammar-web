package grammar.adminmode

import scala.scalajs.js
import japgolly.scalajs.react.vdom.all.{dangerouslySetInnerHtml}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, Ref, BackendScope}

/**
 * Created by Mikael on 13.08.2015.
 */

case class EditableField(description: String, onSave: (String => Unit), multiline: Boolean = false) {
  import EditableField._
  def build = editableField(this)
  def build(modifiers: (editableField.type => editableField.type)) = modifiers(editableField)(this)
}
object EditableField {
  case class State(edit: Boolean, description: String)
  case class Backend($: BackendScope[EditableField, State]) {
    protected val _fieldInput = Ref.to(EditableFieldInput.editableFieldInput, AdminMode.fieldInputKey)
    def fieldInput = _fieldInput($)
    var changeTimer: js.timers.SetTimeoutHandle = null

    def onChange(e: String) = {
      $.modState((s: State) => s.copy(description = e), () => {
        js.timers.clearTimeout(changeTimer)
        changeTimer = js.timers.setTimeout(1000){
          save()
        }
      })
    }

    def goToEditMode() = $.modState((s: State) => s.copy(edit = true), () => {
      fieldInput.tryFocus()
    })
    def save() = {
      val value = fieldInput.get.backend.saveAndGetContent() match {
        case Some(value) =>
          $.props.onSave(value)
        case None =>
      }
    }

    def goFromEditMode() = {
      save()
      $.modState((s: State) => s.copy(edit = false))
    }
  }
  private val editableField = ReactComponentB[EditableField]("Editable span")
    .initialStateP( props => State(edit=false, props.description))
    .backend(Backend)
    .render( (P, S, B) =>
    <.span(
      <.span(
        ^.minWidth := "2em",
        ^.minHeight := "1em",
        ^.`class` := "admin-editable",
        ^.onClick --> B.goToEditMode(),
        ^.display := (if(!S.edit) "inline-block" else "none"),
        dangerouslySetInnerHtml(S.description)),
      EditableFieldInput(
        onBlur = B.goFromEditMode,
        display = S.edit,
        value = P.description,
        onChange = B.onChange,
        multiline = P.multiline
      ).buildWithRef(AdminMode.fieldInputKey)
    )
    ).build
}

