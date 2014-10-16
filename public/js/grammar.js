var editor = null;

var licenceAgreementTitle = "Consent for Data Collection";
var licenceAccept = "I accept";
var licenceDecline = "I decline";
var licenceAgreement = "I hereby accept that my data are recorded for research purpose and for improving the system, " +
		"provided that all data are anonymized.<br/> By Clicking on '"+licenceAccept+"', I accept a cookie lasting for 1 week.";
var LICENCE_COOKIE_DAYS_EXPIRE = 7;
var LICENCE_COOKIE = "LicenceCookie";
var LICENCE_COOKIE_ACCEPTED = "LicenseAccepted";

/** Loads when the document is ready */
$(document).ready(function() {
    editor = ace.edit("codebox");
    var aceRange = ace.require("ace/range").Range;
    ace.require("ace/token_tooltip");
    editor.setTheme("ace/theme/chrome");
    editor.getSession().setMode("ace/mode/scala")
    editor.getSession().setUseWrapMode(true)
    editor.setShowPrintMargin(false);
    editor.setAutoScrollEditorIntoView();
    editor.setHighlightActiveLine(false);
    editor.getSession().setTabSize(2);
    
    function setCookie(cname, cvalue, exdays) {
      var d = new Date();
      d.setTime(d.getTime() + (exdays*24*60*60*1000));
      var expires = "expires="+d.toUTCString();
      document.cookie = cname + "=" + cvalue + "; " + expires;
    }
    
    function getCookie(cname) {
      var name = cname + "=";
      var ca = document.cookie.split(';');
      for(var i=0; i<ca.length; i++) {
          var c = ca[i];
          while (c.charAt(0)==' ') c = c.substring(1);
          if (c.indexOf(name) != -1) return c.substring(name.length,c.length);
      }
      return "";
    }
    
    if(getCookie(LICENCE_COOKIE) != LICENCE_COOKIE_ACCEPTED) {
      var dialogDiv = $('<div>').addClass("ui-dialog");
      document.cookie
      var backgroundDiv = $('<div>').addClass("background-dialog")
      $("body").prepend(backgroundDiv);
      backgroundDiv.append(dialogDiv);
      dialogDiv.append($('<h3>').text(licenceAgreementTitle))
      dialogDiv.append(licenceAgreement+"<br/>");
      var positiveButton = $('<div>').addClass("button positive").text(licenceAccept);
      var negativeButton = $('<div>').addClass("button negative").text(licenceDecline);
      dialogDiv.append(positiveButton)
      dialogDiv.append(negativeButton)
      positiveButton.click(function() {
        setCookie(LICENCE_COOKIE, LICENCE_COOKIE_ACCEPTED, LICENCE_COOKIE_DAYS_EXPIRE);
        backgroundDiv.remove();
      });
      negativeButton.click(function() {
        dialogDiv.remove();
        $("body").animate({opacity: 0}, {duration: 400, complete: function() { $("body").empty(); }})
      });
    }

    var hash = window.location.hash

    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var leonSocket = null

    var headerHeight = $("#title").height()+20

    var lastMarker = -1;

    /** Menu buttons can enable or disable targets, e.g. synthesize window. */
    $(".menu-button").click(function(event) {
        var target = $(this).attr("ref")
        var sel = "#"+target

        if ($(sel).is(":visible")) {
            $(sel).hide()
            $(this).addClass("disabled")
        } else {
            $(sel).show()
            $(this).removeClass("disabled")
        }

    });

    /** Save menu button */
    $("#button-save").click(function(event) {
        recompile()
        event.preventDefault()
    });

    /** Undo menu button */
    $("#button-undo").click(function(event) {
        if (!$(this).hasClass("disabled")) {
            doUndo()
        }
        event.preventDefault()
    });

    /** Redo menu button */
    $("#button-redo").click(function(event) {
        if (!$(this).hasClass("disabled")) {
            doRedo()
        }
        event.preventDefault()
    });

    /** Returns true if it the page allows for local storage */
    function hasLocalStorage() {
      try {
        return 'localStorage' in window && window['localStorage'] !== null;
      } catch (e) {
        return false;
      }
    }

    var handlers = [];
    var compilationStatus = 0
    var searchFinished = false
    var context = "unknown";

    // Undo/Redo
    var backwardChanges = []
    var forwardChanges  = []

    function doUndo() {
      forwardChanges.push(editor.getValue());
      var code = backwardChanges.pop();
      editor.setValue(code)
      editor.selection.clearSelection();
      editor.gotoLine(0);
      recompile();
      updateUndoRedo()
    }

    function doRedo() {
      backwardChanges.push(editor.getValue());
      var code = forwardChanges.pop();
      editor.setValue(code)
      editor.selection.clearSelection();
      editor.gotoLine(0);
      recompile();
      updateUndoRedo()
    }

    function storeCurrent(code) {
      forwardChanges = []
      if (backwardChanges.length >= 1) {
        if (code != backwardChanges[backwardChanges.length-1]) {
          backwardChanges.push(code)
        }
      } else {
          backwardChanges.push(code)
      }
      updateUndoRedo()
    }

    function updateUndoRedo() {
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
    handlers["editor"] = function (data) {
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

    handlers["notification"] = function (data) {
        notify(data.content, data.type);
    }
    
   /* function enableConsoleButtons() {
    	$("#button-check").removeClass("disabled")
    	$("#button-hints").removeClass("disabled")    	
    }
    
    function disableConsoleButtons() {    	
    	$("#button-check").addClass("disabled")
    	$("#button-hints").addClass("disabled")
    }*/

    handlers["console"] = function (data) {
        var txt = $("#console")
        txt.append("===============\n")
        txt.append(data.message+"\n");        
        txt.scrollTop(txt[0].scrollHeight - txt.height())       
        addFeedback(data.message)        
    }
    
    handlers["helpmsg"] = function (data) {               
       addFeedback(data.message, "Input Syntax", true)        
    }
        

    var receiveEvent = function(event) {
        var data = JSON.parse(event.data)
        if (data.kind in handlers) {
            handlers[data.kind](data);
        } else {
            console.log("Unknown event type: "+data.kind)
            console.log(data)
        }
    }

    var connected = false

    var closeEvent = function(event) {
        if (connected) {
            setDisconnected()
        }
    }
    
    //disable, enable even handler
    handlers["disableEvents"] = function(data){
    	$.each(data, function(field, value) {  		      		
  			if(field == "normalize")
  				$('#button-norm').addClass("disabled")
  			else if(field == "getHints")  				
  				$('#button-hints').addClass("disabled")
  			else if(field == "doCheck")  				
  				$('#button-check').addClass("disabled")
  			//add more events here if necessary  		      		  
  	  });
    }
    
    handlers["enableEvents"] = function(data){    	
    	$.each(data, function(field, value) {     		
  			if(field == 'normalize')
  				$('#button-norm').removeClass('disabled')
  			else if(field == 'getHints')
  				$('#button-hints').removeClass('disabled')
  			else if(field == "doCheck")  				
  				$('#button-check').removeClass("disabled")
  			//add more events here if necessary  		      		  
  	  });
    }
    
  //adding options to the downdown list
    handlers["exerciseTypes"] = function(data) {    	
      $('#exercise-select').empty()      
	  $.each(data, function(field, exerciseType) {
		  if(field != "kind") {				  
		    $('#exercise-select').append($('<option></option>').val(field).html(exerciseType));
		  }
	  });      
      //load the problems of the selected exercise
      loadProblems()
    }
    
    //adding options to the downdown list
    handlers["problems"] = function(data) {    	
      $('#example-loader').empty()
      $('#example-loader').append($('<option value="" selected="selected">--Select a problem--</option>'))
	  $.each(data, function(field, exerciseName) {
		  if(field != "kind") {				  
		    $('#example-loader').append($('<option></option>').val(field).html(exerciseName));
		  }
	  });
      $('#example-loader').prop('disabled', false)
    }
    
    // Used to merge feedbacks if they are too close.
    var lastTitle = "";
    var lastTime = 0;
    var eventTitle = "Output";
    
    /** Adds a feedback to the feedback column, and fade/removes the old ones */
    var addFeedback = function(text, title, htmlstring) {
      if(typeof title == "undefined") title = eventTitle;
      var newTime = new Date().getTime();
      /*if(title == lastTitle && newTime - lastTime < 400) {
        // If it is the same feedback/
        var prevFeedback = $("#feedbackcolumn .action .feedback").first()
        prevFeedback.text(prevFeedback.text() + "\n" + text);
        return;
      }*/
      lastTime = newTime;
      lastTitle = title;
    
      var feedback = $("<div>").addClass("action");
      var close = $("<div>").text("X").addClass("closeButton").css("position","absolute").css("right","0px").css("z-index","1000").click(
    		  (function(f) {return (function() {f.remove();});})(feedback));
      feedback.append(close)
      if(typeof title !== "undefined") {
        feedback.append($("<h3>").text(title));
      }
      var divelem = $("<div>").addClass("feedback")      
      if(htmlstring == true)
    	  feedback.append(divelem.html(text));
      else
    	  feedback.append(divelem.text(text));
    	  
      feedback.click(function() { $(this).css("opacity", 1);});
      feedback.insertAfter($("#feedbackcolumn #notifications"))
      feedback.hide();
      //.prepend(feedback);
      setTimeout( function() {
        feedback.show("blind");      
        $("#feedbackcolumn").find(".action").each(function(index, elem) {
	        if(index >= 1) {
	        	$(elem).remove();
	        }
        });
        /*setTimeout( function() {
          $("#feedbackcolumn").find(".action").each(function(index, elem) {
            if(index >= 1) {            	
              $(elem).hide("blind")
              //setTimeout(function() { $(elem).remove(); }, 400);
            } else {
              $(elem).animate({opacity: 1.0/(index + 2)}, 500);
            }
          });
        }, 300);*/
      }, 50);
    }

    var openEvent = function(event) {
      setConnected()
      leonSocket.onmessage = receiveEvent;
      var msg = JSON.stringify(
        {action: "hello"}
      )
      leonSocket.send(msg)
      msg = JSON.stringify({action: "getExerciseTypes"})        
      leonSocket.send(msg)
    }

    var lastReconnectDelay = 0;
    var reconnectIn = 0;

    var reconnectEvent = function(event) {
        setConnected()
        leonSocket.onmessage = receiveEvent;

        notify("And we are back online!", "success")
        recompile()
    }

    function setDisconnected() {
        connected = false
        lastReconnectDelay = 5;
        reconnectIn = lastReconnectDelay;

        checkDisconnectStatus()
    }

    function setConnected() {
        connected = true

        $("#connectError").hide();
        $("#disconnectError").hide();

        lastReconnectDelay = 0;
        reconnectIn = -1;
    }

    function checkDisconnectStatus() {
        if (reconnectIn == 0) {
            reconnectIn = -1;
            $("#disconnectError #disconnectMsg").html("Attempting reconnection...");

            connectWS()
            leonSocket.onmessage = reconnectEvent

            // If still not connected after 2 seconds, consider failed
            setTimeout(function() {
                if (!connected) {
                    if (lastReconnectDelay == 0) {
                        lastReconnectDelay = 5;
                    } else {
                        lastReconnectDelay *= 2;
                    }

                    reconnectIn = lastReconnectDelay;
                }
            }, 2000);
        } else if (reconnectIn > 0) {
            $("#disconnectError #disconnectMsg").html('Retrying in '+reconnectIn+' seconds... <button id="tryReconnect" class="btn btn-danger btn-mini">Try now</button>');

            $("#tryReconnect").click(function() {
                reconnectIn = 0;
                checkDisconnectStatus();
            })

            $("#disconnectError").show().alert();

            reconnectIn -= 1;
        }
    }

    setInterval(function () { checkDisconnectStatus() }, 1000);

    connectWS()
    setTimeout(function() {
        if (!connected) {
            $("#disconnectError").hide();
            $("#connectError").show().alert();
        }
    }, 3000);

    function connectWS() {
        leonSocket = new WS(_websocket_url)
        leonSocket.onopen = openEvent
        leonSocket.onclose = closeEvent
        leonSocket.onerror = function(event) {
          console.log("ERROR")
          console.log(event)
        }
    }

    var lastChange      = 0;
    var lastSavedChange = lastChange;
    var timeWindow      = 2000;

    function updateSaveButton() {
        var e = $("#button-save")
        if (lastChange == lastSavedChange) {
           e.addClass("disabled"); 
        } else {
           e.removeClass("disabled"); 
        }
    }

    function notify(content, type, fade) {
        if (!fade) {
            fade = 3000
        }

        var note = $("<div>", {
            "class": "alert fade in alert-"+type
        }).html('<button type="button" class="close" data-dismiss="alert">&times;</button>'+content)

        $("#notifications").append(note);

        setTimeout(function() {
            note.hide();
        }, fade)
    }

    var oldCode = ""
    function save(currentCode) {    	
        if (oldCode != "" && oldCode != currentCode) {
            if (forwardChanges.length == 0) {
                storeCurrent(oldCode)
            }
        }        
        oldCode = currentCode;
        lastSavedChange = lastChange;
        updateSaveButton();
    }
    
    function recompile() {
    	currentCode = editor.getValue()
        save(currentCode)
        if (connected) {
            var msg = JSON.stringify(
              {action: "doUpdateCode", code: currentCode}
            )            
            leonSocket.send(msg)
        }
    }
    
    function onCodeUpdate() {
        var now = new Date().getTime()

        if (lastChange < (now - timeWindow)) {
            lastChange = new Date().getTime();
            if (lastChange > 0) {
                recompile()
            }
        }
        localStorage.setItem("editorCode", editor.getValue());
    }
    
    handlers["exerciseDesc"] = function(data) {
    	$('#desc').empty()
    	$('#desc').html('<h3 class="std-background"><i class="icon-book"></i> Description:</h3>' +
    						'<div id="desc-space">'+ data.desc +'</div>')    	    	
    }
    

    function loadProblems() {
      var exid = $('#exercise-select').find(":selected").val()
      if (exid == ""){
        $('#example-loader').prop('disabled', true)
        //notify("Excercise not selected!", "error")
      }
      else {
        msg = JSON.stringify({
          action : "getProblemList",
          exerciseId : exid
        })
        leonSocket.send(msg);        
        //doHelp() should we enable this ?
      }
    }

    function loadSelectedExample() {
        var exid = $('#exercise-select').find(":selected").val()
        var pid = $('#example-loader').find(":selected").val()        
        var msg = JSON.stringify({action: "loadExercise", exerciseId : exid, problemId: pid})
        leonSocket.send(msg)
    }

    function loadExample(group, id) {
        if (id) {
            $.ajax({
              url: _leon_prefix+'/ajax/getExample/'+group+'/'+id,
              dataType: "json",
              success: function(data, textStatus, jqXHR) {
                if (data.status == "success") {
                    storeCurrent(editorSession.getValue())
                    editor.setValue(data.code);
                    editor.selection.clearSelection();
                    editor.gotoLine(0);
                    recompile();
                    $("#example-loader").get(0).selectedIndex = 0;
                } else {
                    notify("Loading example failed :(", "error")
                }
              },
              error: function(jqXHR, textStatus, errorThrown) {
                notify("Loading example failed :(", "error")
              }
            });
        }
    }
    
    $("#exercise-select").change(loadProblems);
    $("#example-loader").change(loadSelectedExample);

    var editorSession = editor.getSession();

    editor.commands.addCommand({
        name: 'save',
        bindKey: {win: 'Ctrl-S',  mac: 'Command-S'},
        exec: function(editor) {
            recompile()
        },
        readOnly: true
    });

    editor.commands.removeCommand('replace');
    editor.commands.removeCommand('transposeletters');

    editorSession.on('change', function(e) {
        lastChange = new Date().getTime();
        updateSaveButton();
        setTimeout(onCodeUpdate, timeWindow+50)
    });

    function resizeEditor() {

        var h = $(window).height()-$("#title").height()-6
        //var w = $(window).width()
        var w = $("#codecolumn").width()

        $('#codecolumn').height(h);
        $('#actionscolumn').height(h);
        $('#feedbackcolumn').height(h);
        $('#leoninput').height(h).width(w);
        $('#codebox').height(h).width("100%");

        editor.resize();
    };

    $(window).resize(resizeEditor);

    resizeEditor();

    handlers["replace_grammar"] = function(data) {
        storeCurrent(editorSession.getValue())
        editorSession.setValue(data.grammar)        
    }

    var storedCode = localStorage.getItem("editorCode")

    if (storedCode != null) {
        editor.setValue(storedCode);
        editor.selection.clearSelection();
        editor.gotoLine(0);
    }
    
    $("#button-norm").hover(function() {
		$(this).attr("title","Removes Epsilon, Unit productions and makes the start symbol appear only on the left side");
			}, function() {
		$(this).attr("title","");
	});
    
    $("#button-norm").click(function(event) {
    	if (!$(this).hasClass("disabled")) {
	      var currentCode = editor.getValue()     
	      //first save the state
	      save(currentCode)
	      var msg = JSON.stringify(
	        {action: "normalize", code : currentCode }
	      )
	      leonSocket.send(msg)
    	}
    	event.preventDefault() 
    });
       
      
    function doCheck() {
      eventTitle = "Solution check";    	       
      var currentCode = editor.getValue()
      //first save the state
      save(currentCode)
      //get 'id' of the selected problem
  	  var exid = $('#exercise-select').find(":selected").val()
  	  var pid = $('#example-loader').find(":selected").val()
        //var pid = $('#example-loader').find(":selected").val()
  	  if(exid == "")
  		  notify("Excercise not selected!", "error")
        if(pid == "")
      	  notify("Problem not selected!", "error")
        else {          
  	      var msg = JSON.stringify(
  	        {action: "doCheck", exerciseId: exid, problemId : pid, code : currentCode }
  	      )
  	      leonSocket.send(msg)          
        }
    }    
    
    function requestHint() {
    	eventTitle = "Hint";          	    	  	
        var currentCode = editor.getValue()
        //first save the code
        save(currentCode)
        var pid = $('#example-loader').find(":selected").val()
        if(pid == "")
      	  notify("Problem not selected!", "error")
        else {          
  	      var msg = JSON.stringify(
  	        {action: "getHints", problemId: pid, code : currentCode }
  	      )
  	      leonSocket.send(msg)  	      
        }
    }
    
    $("#button-check").click(function(event) {
    	if (!$(this).hasClass("disabled")) {
            doCheck()
        }
        event.preventDefault() 
    });
    
    $("#button-hints").click(function(event) {
    	if (!$(this).hasClass("disabled")) {
            requestHint()
        }
        event.preventDefault()      
    });
    
    $("#button-abort").click(function(event) {    
        eventTitle = "Abort";        
        var extype = $('#exercise-select').find(":selected").val()
        if(extype == "")
      	  notify("Select the exercise that you were solving!", "error")
        else {          
  	      var msg = JSON.stringify({action: "abortOps", exerciseId: extype })  	        
  	      leonSocket.send(msg)
        }
    });
    
    function doHelp() {
		var extype = $('#exercise-select').find(":selected").val()
	    if(extype == "")
	  	  notify("Select the exercise for which you want help!", "error")
	    else {          
	      var msg = JSON.stringify({action: "getHelp", exerciseId: extype })  	        
	      leonSocket.send(msg)
	    }
    }
    
    $('#button-help').click(function(event) {
    	eventTitle = "help";        
        doHelp()
    });
});
