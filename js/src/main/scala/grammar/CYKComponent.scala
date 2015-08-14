package grammar

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.jquery._
import org.scalajs.jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => l}
import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js.ArrayOps

/**
 * Created by Mikael on 13.08.2015.
 */
case class CYKTable(initialWord: List[String], nonTerminals: Array[String], storeInput: (Int, Int, String) => Unit) {
  import CYKTable._
  def build = cykTable(this)
}

object CYKTable {
  def tdEmpty = <.td(^.className := "cyk-empty")

  val cykTable = ReactComponentB[CYKTable]("CYK table")
  .render(P => {
    val n = P.initialWord.length
    <.table(^.className := "cyk-table", <.tbody(
      <.tr(^.className := "cyk-header",
        P.initialWord.map(value => <.td(value, ^.width:="5em"))
      ),
      for (length <- 1 to n;
           numRows = Math.min(length, n + 1 - length);
           nthRowLocal <- 1 to numRows) yield
      <.tr(
        for (u <- 1 until nthRowLocal) yield tdEmpty,
        for (k <- nthRowLocal to (n - length + 1) by length)
          yield CYKTableInput(k, k + length - 1, P.initialWord.drop(k-1).take(length).mkString(" "), P.nonTerminals,
              (input: String) => {
                P.storeInput(k, k+length-1, input)
              }
          ).build,
        for (u <- 1 to ((n - nthRowLocal + 1) % length)) yield tdEmpty
      )
    ))
  }).build
}
case class CYKTableInput(start: Int, end: Int, word: String, autocomplete: Array[String], storeInput: String => Unit) {
  import CYKTableInput._
  def build = cykTableInput(this)
}
object CYKTableInput {
  import JQueryExtended._
  
  import ArrayOps._
  import scala.collection.mutable.ArrayOps._
  def split( v: String ): js.Array[String] = js.Array(v.split(",\\s*", -1): _*)
  def extractLast( term: String ): String = split( term ).pop()
  def removeTrailingComma(v: String): String = {
     "[,\\s]+$".r.replaceAllIn(v, "")
  }
  def oneTrailingComma(v: String): String = {
    val r = removeTrailingComma(v)
    r + ", "
  }
  case class State(value: String)
  case class Backend(val $: BackendScope[CYKTableInput,State]) {

  }
  val cykTableInput = ReactComponentB[CYKTableInput]("CYK input")
    .initialState(State(""))
    .backend(Backend)
    .render((P, S, B) => {
    import P._
    val length = P.end - P.start + 1
    val id = s"cyk$start-$end"
    val current_terms = split(S.value)
    val autocomplete_array = js.Array((P.autocomplete.filter { x => current_terms.indexOf(x) == -1 }): _*)
    <.td(
      ^.colSpan := length,
      <.input(
        ^.id := id,
        ^.className := "cyk-input",
        ^.`type` := "text",
        ^.value := S.value, 
        ^.width := "100%",
        ^.boxSizing := "border-box",
        ^.title := "Non-terminals generating " + P.word,
        ^.onBlur --> {
            B.$.modState(s => s.copy(removeTrailingComma(s.value)))
            $("#" + id).autocomplete("close") 
          },
          ^.onChange ==> ((e: ReactEventI) => {
            val value = e.target.value
            console.log("changed to "+value)
            val terms = value.split(",\\s*")
            storeInput(terms.mkString(","))
            B.$.modState(s => s.copy(value))
          }),
          ^.onClick --> {
            val v = $("#" + id).value().asInstanceOf[String]
            if(v != "") {
              $("#" + id).value(oneTrailingComma(v))
            }
            $("#" + id).autocomplete(l(
              minLength = 0,
              source = /*js.Array(Array("A","B","AB") : _*).asInstanceOf[js.Any], */ (request: js.Dynamic, response: js.Dynamic) => {
                response($.ui.autocomplete.filter(
                  autocomplete_array, extractLast(request.term.asInstanceOf[String])))
              },
              focus = true,
              select = ((that: js.Dynamic, event: JQueryEventObject, ui: js.Dynamic) => {
                var initialValue = that.value.asInstanceOf[String]
                var terms = split(initialValue)
                // remove the current input
                terms.pop()
                // add the selected item
                terms.push(ui.item.value.asInstanceOf[String])
                // add placeholder to get the comma-and-space at the end
                storeInput(terms.join(","))
                terms.push("")
                val finalValue = terms.join(", ")
                that.value = finalValue
                B.$.modState(s => s.copy(finalValue))
                false
              }): js.ThisFunction)).autocomplete("search", "")
          }
    )
    )
  }).build
}