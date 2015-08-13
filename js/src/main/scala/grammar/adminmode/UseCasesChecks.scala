package grammar.adminmode

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.jquery._
import org.scalajs.jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}

import scala.scalajs.js

/**
 * Created by Mikael on 13.08.2015.
 */

case class UseCasesChecks(all_usecases: Array[(String, String)], use_cases: Array[String], onSave: String => Unit) {
  import UseCasesChecks._
  def build = useCasesChecks(this)
}
object UseCasesChecks {
  case class UseCasesChecksState(use_cases: Array[String])
  case class UseCasesChecksBackend($: BackendScope[UseCasesChecks, UseCasesChecksState]) {
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
    .backend(UseCasesChecksBackend)
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