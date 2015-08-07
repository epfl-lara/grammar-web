package grammar

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.{JSON, JSApp}
import org.scalajs.dom
import org.scalajs.jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}
//import com.scalawarrior.scalajs.ace._
import js.Dynamic.{global => g, literal => l}
import org.scalajs.dom._

trait HandlerDataArgument extends js.Any {
  var message: String = js.native
  var content: String = js.native
  var `type`: String = js.native
  var grammar: String = js.native
  var solution: String = js.native
  var desc: String = js.native
}

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

object GrammarApp extends JSApp {

  import JQueryExtended._

  //def ace: js.Dynamic = g.ace

  // TODO: use ACE editor
  //var editor: js.Dynamic = null // TODO: use ACE editor

  val licenceAgreementTitle = "Consent for Data Collection"
  val licenceAccept = "I accept"
  val licenceDecline = "I decline"
  val licenceAgreement = s"I hereby accept that my data are recorded for research purpose and for improving the system, " +
    "provided that all data are anonymized.<br/> By Clicking on '$licenceAccept', I accept a cookie lasting for 1 week."
  val LICENCE_COOKIE_DAYS_EXPIRE = 7
  val LICENCE_COOKIE = "LicenceCookie"
  val LICENCE_COOKIE_ACCEPTED = "LicenseAccepted"

  def setCookie(cname: String, cvalue: String, exdays: Int): Unit = {
    var d = new js.Date()
    d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000))
    var expires = "expires=" + d.toUTCString()
    document.cookie = cname + "=" + cvalue + "; " + expires
  }

  def getCookie(cname: String): String = {
    val name = cname + "="
    var ca: Array[String] = document.cookie.split(";")
    for (i <- 0 until ca.length) {
      var c: String = ca(i)
      while (c.substring(0, 1) == " ") c = c.substring(1)
      if (c.indexOf(name) != -1) return c.substring(name.length, c.length)
    }
    return ""
  }

  /** Loads when the document is ready */
  $(document).ready(() => {
    println("Starting the script")
    var editor = ace.edit("codebox")
    println("Continuing the script")
    var aceRange = ace.require("ace/range").Range
    ace.require("ace/token_tooltip")
    editor.setTheme("ace/theme/chrome")
    editor.getSession().setMode("ace/mode/scala")
    editor.getSession().setUseWrapMode(true)
    editor.setShowPrintMargin(false)
    editor.setAutoScrollEditorIntoView()
    editor.setHighlightActiveLine(false)
    editor.getSession().setTabSize(2)


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

    var hash = g.window.location.hash

    //var WS = if (g.window.MozWebSocket != null) g.MozWebSocket else g.WebSocket
    var leonSocket: js.Dynamic = null

    var headerHeight = $("#title").height() + 20

    var lastMarker = -1

    /** Menu buttons can enable or disable targets, e.g. synthesize window. */
    $(".menu-button").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      val target = $(_this).attr("ref")
      val sel = "#" + target

      if ($(sel).is(":visible")) {
        $(sel).hide()
        $(_this).addClass("disabled")
      } else {
        $(sel).show()
        $(_this).removeClass("disabled")
      }

    }): js.ThisFunction)

    // Undo/Redo
    var lastChange = 0.0
    var lastSavedChange = lastChange

    def updateSaveButton() {
      var e = $("#button-save")
      if (lastChange == lastSavedChange) {
        e.addClass("disabled")
      } else {
        e.removeClass("disabled")
      }
    }


    var timeWindow = 2000
    var backwardChanges = js.Array[js.Dynamic]()
    var forwardChanges = js.Array[js.Dynamic]()
    var oldCode: js.Dynamic = null

    var connected = false
    var handlers = js.Dictionary.empty[HandlerDataArgument => Unit]
    var compilationStatus = 0
    var searchFinished = false
    var context = "unknown"

    // Used to merge feedbacks if they are too close.
    var lastTitle = ""
    var lastTime = 0.0
    var eventTitle = "Output"

    var lastReconnectDelay = 0
    var reconnectIn = 0

    def save(currentCode: js.Dynamic) {
      if (oldCode != null && oldCode != currentCode) {
        if (forwardChanges.length == 0) {
          storeCurrent(oldCode)
        }
      }
      oldCode = currentCode
      lastSavedChange = lastChange
      updateSaveButton()
    }


    def recompile() {
      var currentCode = editor.getValue()
      save(currentCode)
      if (connected) {
        var msg = JSON.stringify(
          l(action = "doUpdateCode", code = currentCode)
        )
        leonSocket.send(msg)
      }
    }

    /** Save menu button */
    $("#button-save").click((event: JQueryEventObject) => {
      recompile()
      event.preventDefault()
    })

    /** Undo menu button */
    $("#button-undo").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      if (!$(_this).hasClass("disabled")) {
        doUndo()
      }
      event.preventDefault()
    }): js.ThisFunction)

    /** Redo menu button */
    $("#button-redo").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      if (!$(_this).hasClass("disabled")) {
        doRedo()
      }
      event.preventDefault()
    }): js.ThisFunction)

    /** Returns true if it the page allows for local storage */
    def hasLocalStorage() {
      try {
        /*"localStorage" in g.window && */ g.window.localStorage != null
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

    def storeCurrent(code: js.Dynamic) {
      // TODO: Replace by string
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
      var ub = $("#button-undo")
      var rb = $("#button-redo")

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
      var data = JSON.parse(event.data.asInstanceOf[String])
      if (!js.isUndefined(handlers(data.kind))) {
        handlers(data.kind)(data)
      } else {
        g.console.log("Unknown event type: " + data.kind)
        g.console.log(data)
      }
    }

    def closeEvent(event: JQueryEventObject) {
      if (connected) {
        setDisconnected()
      }
    }

    /* function enableConsoleButtons() {
       $("#button-check").removeClass("disabled")
       $("#button-hints").removeClass("disabled")
     }

     function disableConsoleButtons() {
       $("#button-check").addClass("disabled")
       $("#button-hints").addClass("disabled")
     }*/

    handlers("console") = (data: HandlerDataArgument) => {
      var txt = $("#console")
      txt.append("===============\n")
      txt.append(data.message + "\n")
      txt.scrollTop((txt(0).scrollHeight - txt.height()).toInt)
      addFeedback(data.message)
    }

    handlers("helpmsg") = (data: HandlerDataArgument) => {
      addFeedback(data.message, "Input Syntax", true)
    }

    //disable, enable even handler
    handlers("disableEvents") = (data: HandlerDataArgument) => {
      $.each(data, (fld: js.Any, value: js.Any) => {
        val field = fld.asInstanceOf[String]
        if (field == "normalize")
          $("#button-norm").addClass("disabled")
        else if (field == "getHints")
          $("#button-hints").addClass("disabled")
        else if (field == "doCheck")
          $("#button-check").addClass("disabled")
        else if (field == "checkLL1")
          $("#button-ll1").addClass("disabled")
        else if (field == "checkAmbiguity")
          $("#button-ll1").addClass("disabled")
        //add more events here if necessary
      }.asInstanceOf[js.Any])
    }

    handlers("enableEvents") = (data: HandlerDataArgument) => {
      $.each(data, (fld: js.Any, value: js.Any) => {
        val field = fld.asInstanceOf[String]
        if (field == "normalize")
          $("#button-norm").removeClass("disabled")
        else if (field == "getHints")
          $("#button-hints").removeClass("disabled")
        else if (field == "doCheck")
          $("#button-check").removeClass("disabled")
        else if (field == "checkLL1")
          $("#button-ll1").removeClass("disabled")
        else if (field == "checkAmbiguity")
          $("#button-amb").removeClass("disabled")
        //add more events here if necessary
      }.asInstanceOf[js.Any])
    }


    def loadProblems(): Unit = {
      var exid = $("#exercise-select").find(":selected").value().asInstanceOf[String]
      if (exid == "") {
        $("#example-loader").prop("disabled", true)
        //notification("Excercise not selected!", "error")
      }
      else {
        var msg = JSON.stringify(l(
          action = "getProblemList",
          exerciseId = exid
        ))
        leonSocket.send(msg)
        //doHelp() should we enable this ?
      }
    }

    //adding options to the downdown list
    handlers("exerciseTypes") = (data: HandlerDataArgument) => {
      $("#exercise-select").empty()
      $.each(data, (fld: js.Any, exerciseType: js.Any) => {
        val field = fld.asInstanceOf[String]
        if (field != "kind") {
          $("#exercise-select").append($("<option></option>").value(field).html(exerciseType))
        }
        ().asInstanceOf[js.Any]
      })
      //load the problems of the selected exercise
      loadProblems()
    }

    //adding options to the downdown list
    handlers("problems") = (data: HandlerDataArgument) => {
      $("#example-loader").empty()
      $("#example-loader").append($( """<option value="" selected="selected">--Select a problem--</option>"""))
      $.each(data, (fld: js.Any, exerciseName: js.Any) => {
        val field = fld.asInstanceOf[String]
        if (field != "kind") {
          $("#example-loader").append($("<option></option>").value(field).html(exerciseName))
        }
        ().asInstanceOf[js.Any]
      })
      $("#example-loader").prop("disabled", false)
    }

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

      feedback.click(((_this: dom.html.Element) => $(_this).css("opacity", 1)): js.ThisFunction)
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


    def openEvent(event: JQueryEventObject) {
      setConnected()
      leonSocket.onmessage = receiveEvent _
      var msg = JSON.stringify(
        l(action = "hello")
      )
      leonSocket.send(msg)
      msg = JSON.stringify(l(action = "getExerciseTypes"))
      leonSocket.send(msg)
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

    handlers("notification") = (data: HandlerDataArgument) => {
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
        g.console.log("ERROR")
        g.console.log(event)
      }
    }

    connectWS()
    js.timers.setTimeout(3000) {
      if (!connected) {
        $("#disconnectError").hide()
        $("#connectError").show().alert()
      }
    }

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

    handlers("exerciseDesc") = (data: HandlerDataArgument) => {
      $("#desc").empty()
      $("#desc").html( """<h3 class="std-background"><i class="icon-book"></i> Description:</h3>""" +
        """<div id="desc-space">""" + data.desc + """</div>""")
    }

    handlers("EnterAdminMode") = (data: HandlerDataArgument) => {
      $("#login-input").attr("type", "hidden")
      $("#admin-login").html("")
      //display new buttons here
      $("#button-solve").html( """<i class="icon-thumbs-up"></i> <span>Solve</span>""")
    }

    handlers("RejectAdminAccess") = (data: HandlerDataArgument) => {
      $("#login-input").attr("type", "hidden")
      $("#admin-login").html("")
      $("#admin-mode").html( """<i class=""></i><span>Admin mode</span>""")
      notification("Wrong Password", "error")
    }

    handlers("fullsolution") = (data: HandlerDataArgument) => {
      //save the current editor value so that we can go back to it
      save(editor.getValue())
      editor.setValue(data.solution)
      editor.selection.clearSelection()
    }

    $("#admin-mode").click((event: JQueryEventObject) => {
      eventTitle = "admin mode"
      $("#admin-mode").html("")
      $("#login-input").attr("type", "password")
      $("#admin-login").html( """<i class=""></i><span>Submit</span>""")
      event.preventDefault()
    })

    $("#admin-login").click((event: JQueryEventObject) => {
      eventTitle = "admin login"
      //send the password to the server
      var passwd = $("#login-input").value().asInstanceOf[String]
      if (passwd == "") {
        notification("Enter a password", "error")
      }
      else {
        var msg = JSON.stringify(l(
          action = "adminMode",
          password = passwd
        ))
        leonSocket.send(msg)
      }
      event.preventDefault()
    })



    def loadSelectedExample() {
      var exid = $("#exercise-select").find(":selected").value()
      var pid = $("#example-loader").find(":selected").value()
      var msg = JSON.stringify(l(action = "loadExercise", exerciseId = exid, problemId = pid))
      leonSocket.send(msg)
    }

    $("#exercise-select").change(loadProblems _)
    $("#example-loader").change(loadSelectedExample _)

    var editorSession = editor.getSession()

    def loadExample(group: String, id: Int = 0): Unit = {
      if (id != 0) {
        $.ajax(
          l(
            url = g._leon_prefix + "/ajax/getExample/" + group + "/" + id,
            dataType = "json",

            success = (data: js.Dynamic, textStatus: String, jqXHR: JQueryXHR) => {
              if (data.status.asInstanceOf[String] == "success") {
                storeCurrent(editorSession.getValue())
                editor.setValue(data.code)
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
    ))

    editor.commands.removeCommand("replace")
    editor.commands.removeCommand("transposeletters")

    editorSession.on("change", (e: JQueryEventObject) => {
      lastChange = new js.Date().getTime()
      updateSaveButton()
      js.timers.setTimeout(timeWindow + 50)(onCodeUpdate)
    })

    def resizeEditor() {
      var h = $(g.window).height() - $("#title").height() - 6
      //var w = $(window).width()
      var w = $("#codecolumn").width()

      $("#codecolumn").height(h)
      $("#actionscolumn").height(h)
      $("#feedbackcolumn").height(h)
      $("#leoninput").height(h).width(w)
      $("#codebox").height(h).width("100%")

      editor.resize()
    }

    $(g.window).resize(resizeEditor)

    resizeEditor()

    handlers("replace_grammar") = (data: HandlerDataArgument) => {
      storeCurrent(editorSession.getValue())
      editorSession.setValue(data.grammar)
    }

    var storedCode = g.localStorage.getItem("editorCode")

    if (storedCode != null) {
      editor.setValue(storedCode)
      editor.selection.clearSelection()
      editor.gotoLine(0)
    }

    val buttonNorm = $("#button-norm")

    val hoverIn = (event: JQueryEventObject) => {
      buttonNorm.attr("title", "Removes Epsilon, Unit productions and makes the start symbol appear only on the left side")
    }
    val hoverOut = (event: JQueryEventObject) => {
      buttonNorm.attr("title", "")
    }

    buttonNorm.hover(hoverIn, hoverOut)

    $("#button-norm").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      if (!$(_this).hasClass("disabled")) {
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
      var currentCode = editor.getValue()
      //first save the state
      save(currentCode)
      //get "id" of the selected problem
      var exid = $("#exercise-select").find(":selected").value().asInstanceOf[String]
      var pid = $("#example-loader").find(":selected").value().asInstanceOf[String]
      //var pid = $("#example-loader").find(":selected").val()
      if (exid == "")
        notification("Excercise not selected!", "error")
      if (pid == "")
        notification("Problem not selected!", "error")
      else {
        var msg = JSON.stringify(
          l(action = "doCheck", exerciseId = exid, problemId = pid, code = currentCode)
        )
        leonSocket.send(msg)
      }
    }

    def requestHint() {
      eventTitle = "Hint"
      var currentCode = editor.getValue()
      //first save the code
      save(currentCode)
      var pid = $("#example-loader").find(":selected").value().asInstanceOf[String]
      if (pid == "")
        notification("Problem not selected!", "error")
      else {
        var msg = JSON.stringify(
          l(action = "getHints", problemId = pid, code = currentCode)
        )
        leonSocket.send(msg)
      }
    }

    $("#button-check").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      if (!$(_this).hasClass("disabled")) {
        doCheck()
      }
      event.preventDefault()
    }): js.ThisFunction)

    $("#button-hints").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      if (!$(_this).hasClass("disabled")) {
        requestHint()
      }
      event.preventDefault()
    }): js.ThisFunction)

    $("#button-abort").click((event: JQueryEventObject) => {
      eventTitle = "Abort"
      var extype = $("#exercise-select").find(":selected").value().asInstanceOf[String]
      if (extype == "")
        notification("Select the exercise that you were solving!", "error")
      else {
        var msg = JSON.stringify(l(action = "abortOps", exerciseId = extype))
        leonSocket.send(msg)
      }
    })

    def doHelp() {
      var extype = $("#exercise-select").find(":selected").value().asInstanceOf[String]
      if (extype == "")
        notification("Select the exercise for which you want help!", "error")
      else {
        var msg = JSON.stringify(l(action = "getHelp", exerciseId = extype))
        leonSocket.send(msg)
      }
    }

    $("#button-help").click((event: JQueryEventObject) => {
      eventTitle = "help"
      doHelp()
    })

    $("#button-ll1").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      eventTitle = "LL1 check"
      if (!$(_this).hasClass("disabled")) {
        var currentCode = editor.getValue()
        //first save the state
        save(currentCode)
        var msg = JSON.stringify(
          l(action = "checkLL1", code = currentCode)
        )
        leonSocket.send(msg)
      }
      event.preventDefault()
    }): js.ThisFunction)

    $("#button-amb").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      eventTitle = "Ambiguity check"
      if (!$(_this).hasClass("disabled")) {
        var currentCode = editor.getValue()
        //first save the state
        save(currentCode)
        var msg = JSON.stringify(
          l(action = "checkAmbiguity", code = currentCode)
        )
        leonSocket.send(msg)
      }
      event.preventDefault()
    }): js.ThisFunction)

    $("#button-solve").click(((_this: dom.html.Element, event: JQueryEventObject) => {
      eventTitle = "Solve event"
      if (!$(_this).hasClass("disabled")) {
        //get "id" of the selected problem
        var exid = $("#exercise-select").find(":selected").value().asInstanceOf[String]
        var pid = $("#example-loader").find(":selected").value().asInstanceOf[String]
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
  })


  def main(): Unit = {
    println("Hello world !")
  }
}