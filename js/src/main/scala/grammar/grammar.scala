package grammar

import grammar.adminmode.{UseCasesChecks, EditableField, AdminMode}
import japgolly.scalajs.react.vdom.Attr

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.{JSON, JSApp}
import org.scalajs.dom
import org.scalajs.jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}
import dom.html.Element
import com.scalawarrior.scalajs.ace._
import js.Dynamic.{global => g, literal => l}
import org.scalajs.dom._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.vdom.all.{id, dangerouslySetInnerHtml}
import japgolly.scalajs.react._

trait HandlerDataArgument extends js.Any {
  var kind: String = js.native
  val reference: String = js.native
  var message: String = js.native
  var content: String = js.native
  var `type`: String = js.native
  var grammar: String = js.native
  var solution: String = js.native
  var intro: String = js.native
  var desc: String = js.native // Contains the user-shown description of the problem.
  var description: String = js.native // contains the grammar description
  var initial: js.UndefOr[String] = js.native
  var title: String = js.native
  var word: String = js.native
  var usecases: js.UndefOr[String] = js.native

  var all_usecases: js.UndefOr[String] = js.native
  var new_problem_id: Int = js.native
  
  var feedback_text: String = js.native
  
  // CYK
  var table_feedback: js.UndefOr[String] = js.native
  var table_feedback_correct: js.UndefOr[String] = js.native
  var nonterminals: js.UndefOr[String] = js.native
  var cyk_word: js.UndefOr[String] = js.native
}

object GrammarApp extends JSApp {

  import Shared._
  import JQueryExtended._

  val licenceAgreementTitle = "Consent for Data Collection"
  val licenceAccept = "I accept"
  val licenceDecline = "I decline"
  val licenceAgreement = s"I hereby accept that my data are recorded for research purpose and for improving the system, " +
    "provided that all data are anonymized.<br/> By Clicking on '$licenceAccept', I accept a cookie lasting for 1 week."
  val LICENCE_COOKIE_DAYS_EXPIRE = 7
  val LICENCE_COOKIE = "LicenceCookie"
  val LICENCE_COOKIE_ACCEPTED = "LicenseAccepted"

  object ExerciseMode {
    var current: ExerciseMode = GrammarMode
  }
  sealed trait ExerciseMode { self =>
    def enter() {
      ExerciseMode.current = self
      apply()
    }
    protected def apply()
  }
  object GrammarMode extends ExerciseMode {
    def apply() =  {
      $("#codebox").show()
      $("#cykbox").hide()
    }
  }
  object CYKTableMode extends ExerciseMode {
    def apply() =  {
      $("#codebox").hide()
      $("#cykbox").show()
    }
    var currentTable:ReactComponentU[grammar.CYKTable,grammar.CYKTable.State,grammar.CYKTable.Backend,org.scalajs.dom.raw.Element] = null
    
    private var entered_content: Map[String, String] = Map()

    def storeContent(a: Int, b: Int, nonterminals: String) = entered_content += (s"$a-$b" -> nonterminals)
    def getContent(): String = entered_content.map(keyvalue => keyvalue._1 + ":" + keyvalue._2).mkString("\n")
    import CYKTable.{ ErrorMap, CorrectMap, ErrorCorrectMap }
    private var _setErrorsCorrect =  (errorCorrect: ErrorCorrectMap) => {}
    def setErrorsCorrect(errors: ErrorMap, correct: CorrectMap) = _setErrorsCorrect((errors, correct))
    def render(word: List[String], nonterminals: Array[String], errors: ErrorMap, correct: CorrectMap) = {
      currentTable = CYKTable(word, nonterminals, storeContent, errors, correct, _setErrorsCorrect = _).build
      $("#cykbox").empty()
      React.render(currentTable, $("#cykbox")(0).asInstanceOf[dom.Node])
    }
  }
  
  def setCookie(cname: String, cvalue: String, exdays: Int): Unit = {
    val d = new js.Date()
    d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000))
    val expires = "expires=" + d.toUTCString()
    document.cookie = cname + "=" + cvalue + "; " + expires
  }

  def getCookie(cname: String): String = {
    val name = cname + "="
    for (cr <- document.cookie.split(";");
         c = cr.trim()) {
      if (c.startsWith(name)) return c.substring(name.length)
    }
    ""
  }


  // Used to merge feedbacks if they are too close.
  var lastTitle = ""
  var lastTime = 0.0
  var eventTitle = "Output"

  var all_use_cases: String = ""
  var new_problem_id: Int = 0

  /** Loads when the document is ready */
  $(document).ready(onDocumentReady _)

  /** Adds a feedback to the feedback column, and fade/removes the old ones */
  def addFeedback(text: String, titleArg: String = null, htmlstring: Boolean = false): Unit = {
    var title = titleArg
    if (title == null) title = eventTitle
    var newTime = new js.Date().getTime()
    if (title == lastTitle && newTime - lastTime < 400) {
      //wait for all parts of the same feedback to arrive same feedback/
      var prevFeedback = $("#feedbackcolumn .action .feedback").first()
      prevFeedback.text(prevFeedback.text() + "\n\n" + text)
      return
    }
    lastTime = newTime
    lastTitle = title

    val feedback = $("<div>").addClass("action")
    var close = $("<div>").text("X").addClass("closeButton").css("position", "absolute").css("right", "0px").css("z-index", "1000").click(
      ((f: JQuery) => (() => f.remove()))(feedback))
    feedback.append(close)
    if (title != null) {
      feedback.append($("<h3>").text(title))
    }
    var divelem = $("<div>").addClass("feedback")
    if (htmlstring == true)
      feedback.append(divelem.html(text))
    else
      feedback.append(divelem.text(text))

    feedback.click(((self: Element) => $(self).css("opacity", 1)): js.ThisFunction)
    feedback.insertAfter($("#feedbackcolumn #notifications"))
    feedback.hide()
    //.prepend(feedback);
    js.timers.setTimeout(50) {
      feedback.show("blind")
      $("#feedbackcolumn").find(".action").each((index: js.Any, elem: dom.Element) => {
        if (index.asInstanceOf[Int] >= 1) {
          $(elem).remove()
        }
      }.asInstanceOf[js.Any])
      /*setTimeout( () => {
        $("#feedbackcolumn").find(".action").each(function(index, elem) {
          if(index >= 1) {
            $(elem).hide("blind")
            //setTimeout(() => { $(elem).remove(); }, 400);
          } else {
            $(elem).animate({opacity: 1.0/(index + 2)}, 500);
          }
        });
      }, 300);*/
    }
  }


  var grammarSave: GrammarSave = GrammarSave.None

      
  object Button {
    def norm = $("#button-norm")
    def hints = $("#button-hints")
    def check = $("#button-check")
    def ll1 = $("#button-ll1")
    def solve = $("#button-solve")
    def abort = $("#button-abort")
    def help = $("#button-help")
    def amb = $("#button-amb")
    def save = $("#button-save")
    def undo = $("#button-undo")
    def redo = $("#button-redo")
  }
  
  def onDocumentReady(): Unit = {
    val editor = ace.edit("codebox")
    val aceRange = ace.require("ace/range").Range
    ace.require("ace/token_tooltip")
    editor.setTheme("ace/theme/chrome")
    editor.getSession().setMode("ace/mode/scala")
    editor.getSession().setUseWrapMode(true)
    editor.setShowPrintMargin(false)
    editor.setAutoScrollEditorIntoView()
    editor.setHighlightActiveLine(false)

    if(0==10) { // Test CYK. Put 1 to the right of the 0 to make this false
      CYKTableMode.enter()
      CYKTableMode.render(List("a", "b", "a", "c", "a", "a"), Array("S", "A", "B", "C"), Map((2, 4) -> "error here"), Map())//, (i: Int, j: Int, s: String) => {}).build,
    }

    var editorSession = editor.getSession()
    editorSession.setTabSize(2)
    
    if (getCookie(LICENCE_COOKIE) != LICENCE_COOKIE_ACCEPTED) {
      var dialogDiv = $("<div>").addClass("ui-dialog")

      var backgroundDiv = $("<div>").addClass("background-dialog")
      $("body").prepend(backgroundDiv)
      backgroundDiv.append(dialogDiv)
      dialogDiv.append($("<h3>").text(licenceAgreementTitle))
      dialogDiv.append(licenceAgreement + "<br/>")
      var positiveButton = $("<div>").addClass("button positive").text(licenceAccept)
      var negativeButton = $("<div>").addClass("button negative").text(licenceDecline)
      dialogDiv.append(positiveButton)
      dialogDiv.append(negativeButton)
      positiveButton.click(() => {
        setCookie(LICENCE_COOKIE, LICENCE_COOKIE_ACCEPTED, LICENCE_COOKIE_DAYS_EXPIRE)
        backgroundDiv.remove()
      })
      negativeButton.click(() => {
        dialogDiv.remove()
        $("body").animate(l(opacity = 0), l(duration = 400, complete = () => $("body").empty()))
      })
    }

    var hash = window.location.hash

    var leonSocket: js.Dynamic = null

    var headerHeight = $("#title").height() + 20

    var lastMarker = -1

    /** Menu buttons can enable or disable targets, e.g. synthesize window. */
    $(".menu-button").click(((self: Element, event: JQueryEventObject) => {
      val target = $(self).attr("ref")
      val sel = "#" + target

      if ($(sel).is(":visible")) {
        $(sel).hide()
        $(self).addClass("disabled")
      } else {
        $(sel).show()
        $(self).removeClass("disabled")
      }

    }): js.ThisFunction)

    // Undo/Redo
    var lastChange = 0.0
    var lastSavedChange = lastChange

    def updateSaveButton(): Unit = {
      var e = Button.save
      if (lastChange == lastSavedChange) {
        e.addClass("disabled")
      } else {
        e.removeClass("disabled")
      }
    }


    var timeWindow = 2000
    var backwardChanges = js.Array[String]()
    var forwardChanges = js.Array[String]()
    var oldCode = ""

    var connected = false
    var handlers = js.Dictionary.empty[HandlerDataArgument => Unit]
    var compilationStatus = 0
    var searchFinished = false
    var context = "unknown"



    var lastReconnectDelay = 0
    var reconnectIn = 0

    def save(currentCode: String): Unit = {
      if (oldCode != null && oldCode != currentCode) {
        if (forwardChanges.length == 0) {
          storeCurrent(oldCode)
        }
      }
      oldCode = currentCode
      lastSavedChange = lastChange
      updateSaveButton()
    }


    def recompile(): Unit = {
      var currentCode = editor.getValue()
      save(currentCode)
      if (connected) {
        var msg = JSON.stringify(
          l(action = DO_UPDATE_CODE, code = currentCode)
        )
        leonSocket.send(msg)
      }
    }

    /** Save menu button */
    Button.save.click((event: JQueryEventObject) => {
      recompile()
      event.preventDefault()
    })

    /** Undo menu button */
    Button.undo.click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
        doUndo()
      }
      event.preventDefault()
    }): js.ThisFunction)

    /** Redo menu button */
    Button.redo.click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
        doRedo()
      }
      event.preventDefault()
    }): js.ThisFunction)

    /** Returns true if it the page allows for local storage */
    def hasLocalStorage() = {
      try {
        /*"localStorage" in window("localStorage") && */ window.localStorage != null
      } catch {
        case e: Exception => false
      }
    }

    def doUndo() {
      forwardChanges.push(editor.getValue())
      var code = backwardChanges.pop()
      editor.setValue(code)
      editor.selection.clearSelection()
      editor.gotoLine(0)
      recompile()
      updateUndoRedo()
    }

    def doRedo() {
      backwardChanges.push(editor.getValue())
      var code = forwardChanges.pop()
      editor.setValue(code)
      editor.selection.clearSelection()
      editor.gotoLine(0)
      recompile()
      updateUndoRedo()
    }

    def storeCurrent(code: String) {
      grammarSave match {
        case GrammarSave.Initial =>   saveGrammarProperty(SAVE.initial)(code)
        case GrammarSave.Reference => saveGrammarProperty(SAVE.reference)(code)
        case GrammarSave.None =>
      }

      forwardChanges = js.Array()
      if (backwardChanges.length >= 1) {
        if (code != backwardChanges(backwardChanges.length - 1)) {
          backwardChanges.push(code)
        }
      } else {
        backwardChanges.push(code)
      }
      updateUndoRedo()
    }

    def updateUndoRedo() {
      val ub = Button.undo
      val rb = Button.redo

      if (backwardChanges.length > 0) {
        ub.removeClass("disabled")
      } else {
        ub.addClass("disabled")
      }

      if (forwardChanges.length > 0) {
        rb.removeClass("disabled")
      } else {
        rb.addClass("disabled")
      }
    }

    updateUndoRedo()

    /**
     * Compilation
     */
    /*
        handlers("editor") = function (data) {
            if ("annotations" in data) {
                var session = editor.getSession();

                context = "unknown";

                for (var i = 0; i < data.annotations.length; i++) {
                    var a = data.annotations[i];
                    if (a.type == "verification") {
                        context = "verification";
                    } else if (a.type == "synthesis") {
                        context = "synthesis";
                    }

                    if (a.type != "info" && a.type != "error") {
                        session.addGutterDecoration(a.row, "leon_gutter_"+a.type)
                        a.type = "info";
                    }
                }

                session.setAnnotations(data.annotations);
            }
        }*/


    def receiveEvent(event: JQueryEventObject) {
      var data = JSON.parse(event.data.asInstanceOf[String]).asInstanceOf[HandlerDataArgument]
      if (!js.isUndefined(handlers(data.kind))) {
        handlers(data.kind)(data)
      } else {
        console.log("Unknown event type: " + data.kind)
        console.log(data)
      }
    }

    def closeEvent(event: JQueryEventObject) {
      if (connected) {
        setDisconnected()
      }
    }

    /* function enableConsoleButtons() {
       Button.check.removeClass("disabled")
       Button.hints.removeClass("disabled")
     }

     function disableConsoleButtons() {
       Button.check.addClass("disabled")
       Button.hints.addClass("disabled")
     }*/

    handlers(CONSOLE) = (data: HandlerDataArgument) => {
      var txt = $("#console")
      txt.append("===============\n")
      txt.append(data.message + "\n")
      txt.scrollTop((txt(0).scrollHeight - txt.height()).toInt)
      addFeedback(data.message)
    }

    handlers(HELP_MSG) = (data: HandlerDataArgument) => {
      addFeedback(data.message, "Input Syntax", true)
    }


    //disable, enable even handler
    handlers(DISABLE_EVENTS) = (data: HandlerDataArgument) => {
      $.each(data, (fld: js.Any, value: js.Any) => {
        fld.asInstanceOf[String] match {
          case "normalize" => Button.norm.addClass("disabled")
          case "getHints" => Button.hints.addClass("disabled")
          case "doCheck" => Button.check.addClass("disabled")
          case "checkLL1" => Button.ll1.addClass("disabled")
          case "checkAmbiguity" => Button.amb.addClass("disabled")
          case _ => ().asInstanceOf[js.Any]
          //add more events here if necessary
        }
      })
    }

    handlers(ENABLE_EVENTS) = (data: HandlerDataArgument) => {
      $.each(data, (fld: js.Any, value: js.Any) => {
        fld.asInstanceOf[String] match {
          case "normalize" => Button.norm.removeClass("disabled")
          case "getHints" => Button.hints.removeClass("disabled")
          case "doCheck" => Button.check.removeClass("disabled")
          case "checkLL1" => Button.ll1.removeClass("disabled")
          case "checkAmbiguity" => Button.amb.removeClass("disabled")
          case _ => ().asInstanceOf[js.Any]
          //add more events here if necessary
        }
      })
    }

    def loadProblemsForExercise(exid: String): Unit = {
      val msg = JSON.stringify(l(
        ACTION -> GET_PROBLEM_LIST,
        EXERCISE_ID -> exid
      ))
      leonSocket.send(msg)
    }

    def loadProblems(): Unit = {
      val exid = getCurrentExerciseId()
      if (exid == "") {
        $("#example-loader").prop("disabled", true)
        //notification("Excercise not selected!", "error")
      } else {
        loadProblemsForExercise(exid)
        //doHelp() should we enable this ?
      }
    }
    
    handlers(FEEDBACK) = (data: HandlerDataArgument) => {
      val text = data.feedback_text
      addFeedback(text)
      if(getCurrentExerciseId() == "cyk" && data.table_feedback.isDefined) { // CYK
        val feedback = data.table_feedback.get
        val feedback_correct = data.table_feedback_correct.get
        val errors = if(feedback == "") {
          Map(): CYKTable.ErrorMap
        } else {
          (for(line <- feedback.split("\n");
              split = line.split(":").toSeq;
              Seq(ab,error) = split;
              Seq(a,b) = ab.split("-").toSeq) yield ((a.toInt + 1, b.toInt+1) -> error)).toMap
        }
        val correct = if(feedback_correct == "") {
          Map(): CYKTable.CorrectMap
        } else {
          (for(line <- feedback_correct.split("\n");
              split = line.split(":").toSeq;
              Seq(ab,error) = split;
              Seq(a,b) = ab.split("-").toSeq) yield ((a.toInt + 1, b.toInt+1) -> error)).toMap
        }
        CYKTableMode.setErrorsCorrect(errors, correct)
      }
    }

    //adding options to the downdown list
    handlers(EXERCISE_TYPES) = (data: HandlerDataArgument) => { // This is not the right type here.
      $("#exercise-select").empty()
      $.each(data, (fld: js.Any, exerciseType: js.Any) => {
        val field = fld.asInstanceOf[String]
        if (field != "kind") {
          val value = exerciseType.asInstanceOf[js.Dynamic].key.asInstanceOf[String]
          $("#exercise-select").append($("<option>").value(value).html(exerciseType.asInstanceOf[js.Dynamic].title))
        }
        ().asInstanceOf[js.Any]
      })
      //load the problems of the selected exercise
      loadProblems()
    }

    //adding options to the downdown list
    handlers(PROBLEMS) = (data: HandlerDataArgument) => {
      $("#example-loader").empty()
      $("#example-loader").append($( """<option value="" selected="selected">--Select a problem--</option>"""))
      $.each(data, (fld: js.Any, problemName: js.Any) => {
        val field = fld.asInstanceOf[String]
        if (field != "kind") {
          $("#example-loader").append($("<option></option>").value(field).html(problemName))
        }
        ().asInstanceOf[js.Any]
      })
      $("#example-loader").prop("disabled", false)
    }

    def loadExerciseTypes(): Unit = {
      val msg = JSON.stringify(l(ACTION -> GET_EXERCISE_TYPES))
      leonSocket.send(msg)
    }

    def openEvent(event: JQueryEventObject) {
      setConnected()
      leonSocket.onmessage = receiveEvent _
      val msg = JSON.stringify(
        l(action = "hello")
      )
      leonSocket.send(msg)
      loadExerciseTypes()
    }


    def reconnectEvent(event: JQueryEventObject) {
      setConnected()
      leonSocket.onmessage = receiveEvent _

      notification("And we are back online!", "success")
      recompile()
    }

    def setDisconnected () {
      connected = false
      lastReconnectDelay = 5
      reconnectIn = lastReconnectDelay

      checkDisconnectStatus()
    }

    def setConnected () {
      connected = true

      $("#connectError").hide()
      $("#disconnectError").hide()

      lastReconnectDelay = 0
      reconnectIn = -1
    }

    handlers(NOTIFICATION) = (data: HandlerDataArgument) => {
      notification(data.content, data.`type`)
    }

    def checkDisconnectStatus () {
      if (reconnectIn == 0) {
        reconnectIn = -1
        $("#disconnectError #disconnectMsg").html("Attempting reconnection...")

        connectWS()
        leonSocket.onmessage = reconnectEvent _

        // If still not connected after 2 seconds, consider failed
        js.timers.setTimeout(2000) {
          if (!connected) {
            if (lastReconnectDelay == 0) {
              lastReconnectDelay = 5
            } else {
              lastReconnectDelay *= 2
            }

            reconnectIn = lastReconnectDelay
          }
        }
      } else if (reconnectIn > 0) {
        $("#disconnectError #disconnectMsg").html("Retrying in " + reconnectIn + """ seconds... <button id="tryReconnect" class="btn btn-danger btn-mini">Try now</button>""")

        $("#tryReconnect").click(() => {
          reconnectIn = 0
          checkDisconnectStatus()
        })

        $("#disconnectError").show().alert()

        reconnectIn -= 1
      }
    }

    js.timers.setInterval(1000){
      checkDisconnectStatus()
    }

    def connectWS () {

      leonSocket = if (!js.isUndefined(g.MozWebSocket))
        js.Dynamic.newInstance(g.MozWebSocket)(g._websocket_url)
      else js.Dynamic.newInstance(g.WebSocket)(g._websocket_url)
      leonSocket.onopen = openEvent _
      leonSocket.onclose = closeEvent _
      leonSocket.onerror = (event: JQueryEventObject) => {
        console.log("ERROR")
        console.log(event)
      }
    }

    connectWS()
    js.timers.setTimeout(3000) {
      if (!connected) {
        $("#disconnectError").hide()
        $("#connectError").show().alert()
      }
    }

    /** Shows a notification
      */
    def notification(content: String, `type`: String, fd: Int = 0): Unit = {
      var fade = fd
      if (fade == 0) {
        fade = 3000
      }

      var note = $("<div>", l(
        `class` = "alert fade in alert-" + `type`
      )).html( """<button type="button" class="close" data-dismiss="alert">&times;</button>""" + content)

      $("#notifications").append(note)

      js.timers.setTimeout(fade) {
        note.hide()
      }
    }

    def onCodeUpdate() {
      var now = new js.Date().getTime()

      if (lastChange < (now - timeWindow)) {
        lastChange = new js.Date().getTime()
        if (lastChange > 0) {
          recompile()
        }
      }
      g.localStorage.setItem("editorCode", editor.getValue())
    }

    def saveGrammar(properties: Seq[(String, js.Any)]): Unit = {
      val pid = getCurrentProblemId()
      val msg = JSON.stringify(l.applyDynamicNamed("apply")((Seq[(String, js.Any)](
        ACTION -> SAVE_GRAMMAR,
        WHAT -> properties.map(_._1).mkString(","),
        PROBLEM_ID -> pid
      ) ++ properties) : _* ))
      leonSocket.send(msg)
    }

    def saveGrammarProperty(property: String)(value: String): Unit = {
      saveGrammar(Seq[(String, js.Any)]((property, value)))
      property match {
        case SAVE.title =>
          $("#example-loader option:selected").html(value)
        case _ =>
      }
    }

    def addNewProblem() = {
      val title  ="<i>Problem title</i>"
      val description ="<p>The description of your grammar here</p>"
      val reference = "S -> a | a b S | \"\""
      val initial = "S -> a"
      val word = "a b a"
      val usecases = "all"

      val field = new_problem_id
      new_problem_id += 1
      $("#example-loader").append($("<option selected>").value(field.toString).html(title))
      renderDescription(l(
        reference= reference,
        intro= "[Computer-generated]",
        desc= "[Computer-generated]",
        description = description,
        initial= initial,
        title= title,
        word= word,
        usecases= usecases,
        all_usecases = all_use_cases
      ).asInstanceOf[HandlerDataArgument])
      saveGrammar(Seq[(String, js.Any)](
        SAVE.title -> title,
        SAVE.description -> description,
        SAVE.reference -> reference,
        SAVE.initial -> initial,
        SAVE.word -> word,
        SAVE.usecases -> usecases
      ))
    }

    def deleteProblem(): Unit = {
      val title = $("#example-loader option:selected").text()
      val pid = getCurrentProblemId()
      if(dom.confirm(s"Are you sure you want to delete problem #$pid '${title}'")) {
        var msg = JSON.stringify(l(
          ACTION -> DELETE_PROBLEM,
          PROBLEM_ID -> pid
        ))
        leonSocket.send(msg)
        $("#example-loader option:selected").remove()
        $("#example-loader option:nth-child(1)").attr("selected", "true")
      }
    }

    /** Renders the description of the exercise, plus admin stuff if needed*/
    def renderDescription(data: HandlerDataArgument): Unit = {
      $("#desc").empty()
      import AdminMode._
      val referenceRef = "reference"
      val initialRef = "initial"
      
      case class ExerciseDescriptionBackend($: BackendScope[HandlerDataArgument, GrammarSave]) {
        def setGrammarSaveMode(g: GrammarSave) = {
          js.timers.setTimeout(timeWindow + 500)(grammarSave = g)
          $.setState(g)
        }
      }
      val exercise_description =
        ReactComponentB[HandlerDataArgument]("Exercise description")
          .initialState(GrammarSave.None: GrammarSave)
          .backend(ExerciseDescriptionBackend)
          .render((P, S, B) =>
          <.div(
          <.h3(
            ^.`class` := "std-background",
            <.i(^.`class` := "icon-book"),
            " Description:"),
          <.div(
            id := "desc-space",
            admin ?= <.span(<.b("Title:"),
              adminmode.EditableField(P.title, saveGrammarProperty(SAVE.title)).build,
              <.br(),
              <.b("Edit grammars: "),
              adminmode.Button("reference", () => {
                B.setGrammarSaveMode(GrammarSave.Reference)
                grammarSave = GrammarSave.None
                replaceGrammar(P.reference)}, selected = S == GrammarSave.Reference).buildWithRef(referenceRef),
              adminmode.Button("initial", () => {
                B.setGrammarSaveMode(GrammarSave.Initial)
                val initGrammar = if(P.initial.isDefined) P.initial.get else ""
                grammarSave = GrammarSave.None
                replaceGrammar(initGrammar)
              }, selected = S == GrammarSave.Initial).buildWithRef(initialRef),
              grammarSave != GrammarSave.None ?= adminmode.Button("exit grammar editing mode", () => {
                addFeedback("Exiting grammar save mode")
                B.setGrammarSaveMode(GrammarSave.None)}).build,
              <.br()
            ),
            admin ?= <.b("Exercise intro:"),
            P.intro,
            admin ?= <.br(),
            admin ?= <.b("Description:"),
            !admin ?= <.span(
              id := "desc-desc",
              admin ?= (^.`class` := "admin-editable"),
              dangerouslySetInnerHtml(if(admin) P.description else P.desc)
            ),
            admin ?=  EditableField(P.description, saveGrammarProperty(SAVE.description), multiline = true).build,
            admin ?= <.br(),
            admin ?= <.span(
              <.b("Word:"),
              EditableField( if(!js.isUndefined(P.asInstanceOf[js.Dynamic].word)) P.word else "", saveGrammarProperty(SAVE.word)).build,
              <.br()
            ),
            admin ?= <.span(
              <.b("Use cases:"),
              UseCasesChecks({
                all_use_cases = P.all_usecases.get
                all_use_cases.asInstanceOf[String].split("\n").map { v => {
                  val splitted = v.split(";")
                  (splitted(0), splitted(1))
                }
                }
              }, P.usecases.get.split("\n"), saveGrammarProperty(SAVE.usecases)).build
            )
          )).render
      ).build
      React.render(exercise_description(data), $("#desc")(0).asInstanceOf[dom.Node])
      if(admin) {
        $("#desc-desc").click(() => {
          $("#desc-desc").hide()
          $("#desc-edit").show().focus()
        })
        $("#desc-edit").blur(() => {
          $("#desc-desc").html($("#desc-edit").text()).show()
          $("#desc-edit").hide()
        })
      }
      $("#desc").data("received-data", data)
      if(getCurrentExerciseId() == "cyk") {
        CYKTableMode.enter()
        CYKTableMode.render(data.cyk_word.get.split(" ").toList, data.nonterminals.get.split(","), Map(), Map())
      } else {
        GrammarMode.enter()
      }
    }

    handlers(EXERCISE_DESC) = (data: HandlerDataArgument) => renderDescription(data)

    handlers(ENTER_ADMIN_MODE) = (data: HandlerDataArgument) => {
      $("#login-input").attr("type", "hidden")
      $("#admin-login").html("")
      //display new buttons here
      Button.solve.html( """<i class="icon-thumbs-up"></i> <span>Solve</span>""")

      all_use_cases = data.all_usecases.get
      new_problem_id = data.new_problem_id
      // Re-renders some descriptions
      val exId = getCurrentExerciseId()
      val pid = getCurrentProblemId()
      console.log("setting timer up...")
      js.timers.setTimeout(0){
        loadExerciseTypes()
      }
      if(exId != "") {
        loadProblemsForExercise(exid = exId)
        js.timers.setTimeout(1000){ $(s"#exercise-select option[value=$exId]").attr("selected", true) }
      }
      if(pid != "") {
        js.timers.setTimeout(1000)(loadExerciseFromId(exId, pid))
        js.timers.setTimeout(1000){ $(s"#example-loader option[value=$pid]").attr("selected", true) }
      }
      $("#example-loader").parent()
        .append($("<input type='button' class='admin-button' value='Add new'>").click(addNewProblem _))
        .append($("<input type='button' class='admin-button' value='Delete'>").click(deleteProblem _))

    }

    handlers(REJECT_ADMIN_ACCESS) = (data: HandlerDataArgument) => {
      $("#login-input").attr("type", "hidden")
      $("#admin-login").html("")
      $("#admin-mode").html( """<i class=""></i><span>Admin mode</span>""")
      notification("Wrong Password", "error")
    }

    handlers(FULL_SOLUTION) = (data: HandlerDataArgument) => {
      //save the current editor value so that we can go back to it
      save(editor.getValue())
      editor.setValue(data.solution)
      editor.selection.clearSelection()
    }

    def admin(): Boolean = {
      $("#admin-mode").html() == ""
    }

    $("#admin-mode").click((event: JQueryEventObject) => {
      eventTitle = "admin mode"
      $("#admin-mode").html("")
      $("#login-input").attr("type", "password").on("keyup.password", (e: JQueryEventObject) => {
        if(e.keyCode == key.enter) {
          $("#admin-login").click()
        }
      }).focus()
      $("#admin-login").html( """<i class=""></i><span>Submit</span>""")
      event.preventDefault()
    })

    $("#admin-login").click((event: JQueryEventObject) => {
      eventTitle = "admin login"
      //send the password to the server
      var passwd = $("#login-input").value().asInstanceOf[String]
      if (passwd == "") {
        notification("Enter a password", "error")
      } else {
        var msg = JSON.stringify(l(
          action = ADMIN_MODE,
          password = passwd
        ))
        leonSocket.send(msg)
      }
      event.preventDefault()
    })

    def getCurrentExerciseId(): String = {
      $("#exercise-select").find(":selected").value().asInstanceOf[String]
    }
    def getCurrentProblemId(): String = {
      $("#example-loader").find(":selected").value().asInstanceOf[String]
    }

    def loadExerciseFromId(exid: String, pid: String) {
      if(pid == "" || exid == "") return;
      val msg = JSON.stringify(l(ACTION -> LOAD_EXERCISE, EXERCISE_ID -> exid, PROBLEM_ID -> pid))
      leonSocket.send(msg)
    }

    def loadSelectedExercise() {
      var exid = getCurrentExerciseId()
      var pid = getCurrentProblemId()
      loadExerciseFromId(exid, pid)
    }

    $("#exercise-select").change(loadProblems _)
    $("#example-loader").change(loadSelectedExercise _)

    def loadExample(group: String, id: Int = 0): Unit = {
      if (id != 0) {
        $.ajax(
          l(
            url = g._leon_prefix + "/ajax/getExample/" + group + "/" + id,
            dataType = "json",

            success = (data: js.Dynamic, textStatus: String, jqXHR: JQueryXHR) => {
              if (data.status.asInstanceOf[String] == "success") {
                storeCurrent(editorSession.getValue())
                editor.setValue(data.code.asInstanceOf[js.UndefOr[String]].get)
                editor.selection.clearSelection()
                editor.gotoLine(0)
                recompile()
                $("#example-loader").get(0).selectedIndex = 0
              } else {
                notification("Loading example failed :(", "error")
              }
            },

            error = (jqXHR: JQueryXHR, textStatus: String, errorThrown: Any) => {
              notification("Loading example failed :(", "error")
            }
          ).asInstanceOf[JQueryAjaxSettings]
        )
      }
    }


    editor.commands.addCommand(l(
      name = "save",
      bindKey = l(win = "Ctrl-S", mac = "Command-S"),
      exec = (editor: js.Dynamic) => {
        recompile()
      },
      readOnly = true
    ).asInstanceOf[EditorCommand])

    editor.commands.removeCommand("replace")
    editor.commands.removeCommand("transposeletters")
    editorSession.on("change", (e: js.Any) => {
      lastChange = new js.Date().getTime()
      updateSaveButton()
      js.timers.setTimeout(timeWindow + 50)(onCodeUpdate)
    })

    def resizeEditor() {
      var h = $(window).height() - $("#title").height() - 6
      //var w = $(window).width()
      var w = $("#codecolumn").width()

      $("#codecolumn").height(h)
      $("#actionscolumn").height(h)
      $("#feedbackcolumn").height(h)
      $("#leoninput").height(h).width(w)
      $("#codebox").height(h).width("100%")

      editor.resize()
    }

    $(window).resize(resizeEditor)

    resizeEditor()

    def replaceGrammar(newGrammar: String) = {
      storeCurrent(editorSession.getValue())
      editorSession.setValue(newGrammar)
    }

    handlers(REPLACE_GRAMMAR) = (data: HandlerDataArgument) => {
      replaceGrammar(data.grammar)
    }

    val storedCode  = g.localStorage.getItem("editorCode").asInstanceOf[js.UndefOr[String]]

    if (storedCode.isDefined) {
      editor.setValue(storedCode.get)
      editor.selection.clearSelection()
      editor.gotoLine(0)
    }

    val buttonNorm = Button.norm

    val hoverIn = (event: JQueryEventObject) => {
      buttonNorm.attr("title", "Removes Epsilon, Unit productions and makes the start symbol appear only on the left side")
    }
    val hoverOut = (event: JQueryEventObject) => {
      buttonNorm.attr("title", "")
    }

    buttonNorm.hover(hoverIn, hoverOut)

    Button.norm.click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
        var currentCode = editor.getValue()
        //first save the state
        save(currentCode)
        val msg = JSON.stringify(js.Dynamic.literal(action = "normalize", code = currentCode): js.Any)
        leonSocket.send(msg)
      }
      event.preventDefault()
    }): js.ThisFunction)


    def doCheck() {
      eventTitle = "Solution check"
      val exid = getCurrentExerciseId()
      val pid = getCurrentProblemId()
      ExerciseMode.current match {
        case GrammarMode =>
          Button.ll1.removeClass("disabled")
          Button.amb.removeClass("disabled")
          var currentCode = editor.getValue()
          //first save the state
          save(currentCode)
          //get "id" of the selected problem
          //var pid = $("#example-loader").find(":selected").val()
          if (exid == "")
            notification("Exercise not selected!", "error")
          if (pid == "")
            notification("Problem not selected!", "error")
          else {
            var msg = JSON.stringify(
              l(action = "doCheck", exerciseId = exid, problemId = pid, code = currentCode)
            )
            leonSocket.send(msg)
          }
        case CYKTableMode =>
          Button.ll1.addClass("disabled")
          Button.amb.addClass("disabled")
          val table: String = CYKTableMode.getContent()
          var msg = JSON.stringify(
            l(ACTION -> DO_CHECK, EXERCISE_ID -> exid, PROBLEM_ID -> pid, "code" -> table)
          )
          
          leonSocket.send(msg)
      }
      
    }

    def requestHint() {
      eventTitle = "Hint"
      var currentCode = editor.getValue()
      //first save the code
      save(currentCode)
      var pid = getCurrentProblemId()
      if (pid == "")
        notification("Problem not selected!", "error")
      else {
        var msg = JSON.stringify(
          l(action = "getHints", problemId = pid, code = currentCode)
        )
        leonSocket.send(msg)
      }
    }

    Button.check.click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
        doCheck()
      }
      event.preventDefault()
    }): js.ThisFunction)

    Button.hints.click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
        requestHint()
      }
      event.preventDefault()
    }): js.ThisFunction)

    Button.abort.click((event: JQueryEventObject) => {
      eventTitle = "Abort"
      var extype = getCurrentExerciseId()
      if (extype == "")
        notification("Select the exercise that you were solving!", "error")
      else {
        var msg = JSON.stringify(l(action = "abortOps", exerciseId = extype))
        leonSocket.send(msg)
      }
    })

    def doHelp() {
      var extype = getCurrentExerciseId()
      if (extype == "")
        notification("Select the exercise for which you want help!", "error")
      else {
        var msg = JSON.stringify(l(action = "getHelp", exerciseId = extype))
        leonSocket.send(msg)
      }
    }

    Button.help.click((event: JQueryEventObject) => {
      eventTitle = "help"
      doHelp()
    })

    Button.ll1.click(((self: Element, event: JQueryEventObject) => {
      eventTitle = "LL1 check"
      if (!$(self).hasClass("disabled")) {
        val currentCode = editor.getValue()
        //first save the state
        save(currentCode)
        val msg = JSON.stringify(
          l(action = "checkLL1", code = currentCode)
        )
        leonSocket.send(msg)
      }
      event.preventDefault()
    }): js.ThisFunction)

    Button.amb.click(((self: Element, event: JQueryEventObject) => {
      eventTitle = "Ambiguity check"
      if (!$(self).hasClass("disabled")) {
        val currentCode = editor.getValue()
        //first save the state
        save(currentCode)
        var msg = JSON.stringify(
          l(action = "checkAmbiguity", code = currentCode)
        )
        leonSocket.send(msg)
      }
      event.preventDefault()
    }): js.ThisFunction)

    Button.solve.click(((self: Element, event: JQueryEventObject) => {
      eventTitle = "Solve event"
      if (!$(self).hasClass("disabled")) {
        //get "id" of the selected problem
        var exid = getCurrentExerciseId()
        var pid = getCurrentProblemId()
        if (exid == "")
          notification("Excercise not selected!", "error")
        if (pid == "")
          notification("Problem not selected!", "error")
        else {
          var currentCode = editor.getValue()
          var msg = JSON.stringify(
            l(action = "solve", exerciseId = exid, problemId = pid, code = currentCode)
          )
          leonSocket.send(msg)
        }
      }
      event.preventDefault()
    }): js.ThisFunction)
  }


  def main(): Unit = {
    println("Hello world !")
  }
}