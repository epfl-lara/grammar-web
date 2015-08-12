package grammar

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.vdom.all.{id, dangerouslySetInnerHtml}
import org.scalajs.dom
import org.scalajs.jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}
import scala.scalajs.js
import Shared._
import org.scalajs.dom._

/**
 * Created by Mikael on 10.08.2015.
 */
object AdminMode {

  val fieldInputKey = "fieldInput"

  val adminButton = ReactComponentB[(String, () => Unit)]("Open reference grammar button")
    .render(data =>
    <.input(^.`class` := "admin-button", ^.`type` := "button", ^.value:=data._1, ^.onClick --> {
      data._2()
    }))
    .build

  case class EditableState(value: String, savedValue: String)
  case class EditableFieldInput(onBlur: () => Unit, display: Boolean, value: String, onChange: String => Unit) {
    def build = editableFieldInput(this)
    def build(modifiers: (editableFieldInput.type => editableFieldInput.type)) = modifiers(editableFieldInput)(this)
  }
  class EditableBackend($: BackendScope[EditableFieldInput, EditableState]) {
    def setContent(value: String) = {
      $.modState((s: EditableState) => s.copy(value = value))

    }
    def onChange(e: ReactEventI) = {
      setContent(e.target.value)
    }
    def state = $.state
    def resetContent() = $.modState((s: EditableState) => s.copy(value = s.savedValue))
    def saveAndGetContent() = {
      if($.state.savedValue == $.state.value) {
        None
      } else {
        $.modState((s: EditableState) => s.copy(savedValue = s.value))
        Some(state.value)
      }
    }
  }
  val editableFieldInput = ReactComponentB[EditableFieldInput]("input text for a field")
    .initialStateP((props: EditableFieldInput) => EditableState(props.value, props.value))
    .backend(new EditableBackend(_))
    .render((P, S, B) => {
    <.input(^.`class` := "admin-editable editable-part",
    ^.`type` := "text",
    ^.onChange ==> ((e: ReactEventI) => {B.onChange(e); P.onChange(e.target.value)}),
    ^.onBlur --> P.onBlur(),
    ^.display := (if(P.display) "inline-block" else "none"),
    ^.value := S.value
  )}).build

  case class State(edit: Boolean, description: String)
  case class EditableField(description: String, onSave: (String => Unit)) {
    def build = editableField(this)
    def build(modifiers: (editableField.type => editableField.type)) = modifiers(editableField)(this)
  }
  class Backend($: BackendScope[EditableField, State]) {
    protected val _fieldInput = Ref.to(editableFieldInput, fieldInputKey)
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
    .backend(new Backend(_))
    .render( (P, S, B) =>
      <.span(
        <.span(
          ^.minWidth := "2em",
          ^.minHeight := "1em",
          ^.`class` := "admin-editable",
          ^.onClick --> B.goToEditMode(),
          ^.display := (if(!S.edit) "inline-block" else "none"),
          dangerouslySetInnerHtml(S.description)),
        editableFieldInput.withRef(fieldInputKey)(EditableFieldInput(
          onBlur = B.goFromEditMode,
          display = S.edit,
          value = P.description,
          onChange = B.onChange
        ))
        /*adminButton(("save", () => {
          val value = B.fieldInput.get.state.value
          P.onSave(value)
          B.fieldInput.get.modState(s => s.copy(savedValue = value))
        })),*/
        /*adminButton(("reset", () => {
          val savedValue = B.fieldInput.get.state.savedValue
          B.onChange(savedValue)
          B.fieldInput.get.modState(s => s.copy(value = savedValue))
        }))*/
      ) // Work on reset to make it work.
    ).build

  case class UseCasesChecks(all_usecases: Array[(String, String)], use_cases: Array[String], onSave: String => Unit) {
    def build = useCasesChecks(this)
  }
  case class UseCasesChecksState(use_cases: Array[String])
  class UseCasesChecksBackend($: BackendScope[UseCasesChecks, UseCasesChecksState]) {
    def isAllUseCases(use_cases: Array[String] =  $.state.use_cases): Boolean = {
      val uscontains = use_cases.indexOf(_: String) > -1
      !uscontains("proglang") && (uscontains("all") || $.props.all_usecases.forall{
        case (key, value) =>
          key == "all" || key == "nogrammar" || key == "proglang" || uscontains(key)
      })
    }
    def isAllExceptGrammarUseCases(use_cases: Array[String] =  $.state.use_cases): Boolean ={
      val uscontains = use_cases.indexOf(_: String) > -1
      !uscontains("proglang") && !uscontains("grammar") && (uscontains("nogrammar") || $.props.all_usecases.forall{
        case (key, value) =>
          key == "all" || key == "nogrammar" || key == "proglang" || key == "grammar" || uscontains(key)
      })
    }

    def stateToSave(): String = {
      if(isAllUseCases()) "all" else if(isAllExceptGrammarUseCases()) "nogrammar" else $.state.use_cases.mkString(",")
    }
    var changeTimer: js.timers.SetTimeoutHandle = null

    def setChecked(key: String, checked: Boolean): Unit = {
      key match {
        case "all" =>
          if(checked) {
            $.setState(UseCasesChecksState(use_cases = Array("all")))
          } else {
            $.setState(UseCasesChecksState(use_cases = Array[String]()))
          }
        case "nogrammar" =>
          if(checked) {
            $.setState(UseCasesChecksState(use_cases = Array("nogrammar")))
          } else {
            $.setState(UseCasesChecksState(use_cases = Array[String]()))
          }
        case _ =>
          val current_state = if($.state.use_cases.indexOf("all") > -1) {
            $.props.all_usecases.map(_._1).filter(k => k != "proglang" && k != "all" && k != "nogrammar")
          } else if($.state.use_cases.indexOf("nogrammar") > -1) {
            $.props.all_usecases.map(_._1).filter(k => k != "proglang" && k != "all" && k != "nogrammar" && k != "grammar")
          } else $.state.use_cases

          val contains = (s: UseCasesChecksState, key: String) => s.use_cases.contains(key)
          $.setState($.state.copy(use_cases = $.props.all_usecases.map(_._1).filter(k => {
            (checked && ((k == key) || (k != key && current_state.indexOf(k) > -1))) ||
            (!checked && k != key && current_state.indexOf(k) > -1)
          })))
      }
      js.timers.clearTimeout(changeTimer)
      changeTimer = js.timers.setTimeout(1000){
        $.props.onSave(stateToSave())
      }
    }
  }
  val useCasesChecks = ReactComponentB[UseCasesChecks]("Use case checkboxes")
    .initialStateP( props => UseCasesChecksState(props.use_cases))
    .backend(new UseCasesChecksBackend(_))
    .render{  (P, S, B) => {
    val use_cases = S.use_cases
    val all_use_cases = B.isAllUseCases()
    val all_except_grammar_use_cases = B.isAllExceptGrammarUseCases()
    <.div(P.all_usecases map { case (key, value) => {
      val checked = (
        key != "nogrammar" && key != "proglang" && all_use_cases ||
        key != "all" && key != "proglang"  && key != "grammar"  && all_except_grammar_use_cases ||
          S.use_cases.indexOf(key) > -1)
      <.span(<.input(
        ^.`type` := "checkbox",
        ^.value := key,
        ^.onClick ==> ((e: ReactEvent) => {
          val target = e.target
          val key = $(target).value().asInstanceOf[String]
          val checked = $(e.target).is(":checked")
          B.setChecked(key, checked)
        }),
        ^.checked := checked
        , value
      ), <.br()) } }
    ) } }
    .build

}
