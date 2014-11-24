package models

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent._
import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._
import akka.pattern.ask
import play.api.Play.current
import grammar.GrammarParser
import grammar.exercises._
import grammar.EBNFGrammar.BNFGrammar
import grammar.CFGrammar.Grammar
import grammar.BNFConverter
import parsing.CNFConverter
import grammar.CFGrammar
import java.util.concurrent.Executors
import scala.util.Success
import scala.util.Failure
import grammar.exercises.ExerciseType
import play.Logger
import java.util.Calendar
import java.io.File
import grammar.OperationContext

object database1 extends GrammarDatabase(Play.getFile("/public/resources/GrammarDatabase.xml")) {
}

object Guid {
  private var id: Long = 0
  def getNextId = {
    id = id + 1
    id
  }
}

object AdminPassword {
  /**
   * Reset the password here.
   * TODO: we can use a SHA or Md5 hash here.
   */
  def checkPassword(pass: String) = {
    if (pass == "jiffy")
      true
    else false
  }
}

class WebSession(remoteIP: String) extends Actor {
  import Protocol._
  //creating a thread pool for futures
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

  val eventLogger = {
    val filename = new java.text.SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new java.util.Date())
    val newfile = new File("logs/" + filename + ".log")
    newfile.createNewFile()
    new java.io.PrintWriter(newfile)
  }

  val (enumerator, channel) = Concurrent.broadcast[JsValue]

  def pushMessage(v: JsValue) = channel.push(v)

  def clientLog(msg: String)(implicit msgid: Long) = {
    val timestamp = new java.text.SimpleDateFormat("dd-HH-mm-ss").format(new java.util.Date())
    eventLogger.println("[>][" + msgid + "][" + timestamp + "] " + msg)
    eventLogger.flush()
    pushMessage(toJson(Map("kind" -> "console", "level" -> "log", "message" -> msg)))
  }

  def clientError(msg: String)(implicit msgid: Long) = {
    val timestamp = new java.text.SimpleDateFormat("dd-HH-mm-ss").format(new java.util.Date())
    eventLogger.println("[>][" + msgid + "][" + timestamp + "] " + msg)
    eventLogger.flush()
    pushMessage(toJson(Map("kind" -> "console", "level" -> "error", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    pushMessage(toJson(Map("kind" -> toJson(kind)) ++ data))
  }

  //operation list
  sealed abstract class OpRes
  case class Last(str: String) extends OpRes
  case class Partial(str: String, nextPart: Future[OpRes]) extends OpRes

  //a list operation futures
  var currentOp: Option[(Future[OpRes], OperationContext)] = None
  def recordFuture(opfuture: Future[OpRes], opctx: OperationContext, extype: Option[ExerciseType.ExType])(implicit msgid: Long) {

    currentOp = Some((opfuture, opctx))
    //register a call-back
    opfuture onComplete {
      case Success(opRes) if (currentOp.isDefined && currentOp.get._1 == opfuture) =>
        opRes match {
          case Last(resstr) =>
            //send the message to the client and complete the operation
            clientLog(resstr)
            extype match {
              case Some(ExerciseType.GrammarEx) =>
                enableEvents(List("doCheck", "getHints"))
              case Some(_) =>
                enableEvents(List("doCheck"))
              case _ =>
                //allow every thing here
                enableEvents(List("doCheck", "getHints"))
            }
          case Partial(partRes, nextPart) =>
            //send partial result to the client and continue the next operation
            clientLog(partRes)
            recordFuture(nextPart, opctx, extype)
        }
      //else do nothing            
      case Failure(msg) if (currentOp.isDefined && currentOp.get._1 == opfuture) =>
        clientLog("Operation aborted abnormally")
        //print the exception to the console
        println(msg)

      case _ =>
        //for debugging 
        println(s"operation $msgid terminated")
      //Here, the currentop has been superseeded by another operation 
      //or has been aborted. so ignore the result        
    }
  }

  def disableEvents(events: List[String]) {
    val emptymsg = toJson("")
    event("disableEvents", events.map(_ -> emptymsg).toMap)
  }

  def enableEvents(events: List[String]) {
    val emptymsg = toJson("")
    event("enableEvents", events.map(_ -> emptymsg).toMap)
  }

  //keep track of the mode 
  var adminMode = false

  def receive = {
    case Init =>
      sender ! InitSuccess(enumerator)

    case FromClient(msg) =>
      //create a unique id for message
      implicit val msgid = Guid.getNextId
      try {
        val timestamp = new java.text.SimpleDateFormat("dd-HH-mm-ss").format(new java.util.Date())
        eventLogger.println("[<][" + msgid + "][" + timestamp + "] " + msg)
        eventLogger.flush()

        (msg \ "action").as[String] match {
          case "hello" =>
            clientLog("Welcome!")

          case "adminMode" =>
            if (AdminPassword.checkPassword((msg \ "password").as[String])) {
              adminMode = true
              event("EnterAdminMode", Map())
            } else
              event("RejectAdminAccess", Map())

          case "abortOps" =>
            //abort the current operation
            currentOp match {
              case Some((_, opctx)) =>
                opctx.abort = true
              case _ => ;
            }
            //clear the currentOps
            currentOp = None
            //enable disabled events. However, do not enable more than allowed by the context
            val exid = (msg \ "exerciseId").as[String].toInt
            val exType = ExerciseType.getExType(exid)
            exType match {
              case Some(ExerciseType.GrammarEx) =>
                enableEvents(List("getHints", "doCheck"))
              case Some(_) => enableEvents(List("doCheck"))
              case None =>
                //here, enable all to be safe
                enableEvents(List("getHints", "doCheck"))
            }

          case "doUpdateCode" =>
            ;
          /*val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed to the console)            
            //it is reserved for newly created symbols
            val (bnf, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnf.isDefined)
              clientLog("Parse Error: " + errstr)*/
          case "getExerciseTypes" =>
            //read all exercises and send their names and ids to the clients                        
            val data = ExerciseType.allExercises.map(exType =>
              (exType.id.toString -> toJson(exType.toString))).toMap
            event("exerciseTypes", data)

          case "getProblemList" =>
            //read all problems in the grammar database that suit the exercise selected
            //and send their names and ids to the clients
            val exid = (msg \ "exerciseId").as[String].toInt
            val exType = ExerciseType.getExType(exid)
            if (exType.isDefined) {
              val grammarEntries = database1.entriesForExercise(exType.get)
              //send all grammarEntries
              val data = generateProblemList(grammarEntries).map { case (k, v) => (k -> toJson(v)) }.toMap
              event("problems", data)
              exType.get match {
                //leave doCheck untouched
                case ExerciseType.GrammarEx => enableEvents(List("getHints", "normalize"))
                case _ => disableEvents(List("getHints", "normalize"))
              }
            } else {
              //log error message
              clientLog("Exercise with id: " + exid + " does not exist")
            }

          case "loadExercise" =>
            val pid = (msg \ "problemId").as[String].toInt
            val data = database1.grammarEntries.find(_.id == pid) match {
              case None =>
                Map("desc" -> toJson("There is no grammar in the database with the given id: " + pid))
              case Some(gentry) =>
                val exid = (msg \ "exerciseId").as[String].toInt
                val extype = ExerciseType.getExType(exid)
                //clientLog("Exercise ID: "+exid+" grammar: "+gentry.reference)
                if (extype.isDefined) {
                  val data = Map("desc" -> toJson(generateProblemStatement(gentry, extype.get)))
                  event("exerciseDesc", data)
                } else
                  //log error message
                  clientLog("Exercise with id: " + exid + " does not exist")
            }

          case "doCheck" =>
            val pid = (msg \ "problemId").as[String].toInt
            database1.grammarEntries.find(_.id == pid) match {
              case None =>
                clientLog("There is no grammar in the database with the given id: " + pid)
              case Some(gentry) =>
                val exid = (msg \ "exerciseId").as[String].toInt
                val extype = ExerciseType.getExType(exid)
                if (extype.isDefined) {
                  val userAnswer = (msg \ "code").as[String]
                  //create a future for the operation and add it to the futures list
                  val opctx = new OperationContext()
                  val checkFuture = Future {
                    checkSolution(gentry, extype.get, userAnswer)(opctx)
                  }
                  recordFuture(checkFuture, opctx, extype)
                  //disable check and hints event, leave 'normalize' untouched              
                  disableEvents(List("getHints", "doCheck"))

                } else
                  //log error message
                  clientLog("Exercise with id: " + exid + " does not exist")
            }

          case "checkLL1" =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed in the client console)
            val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnfGrammar.isDefined)
              clientLog("Parse Error:" + errstr)
            else {
              //check for ll1 property              
              clientLog(checkLL1(bnfGrammar.get).get)
            }

          case "checkAmbiguity" =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed in the client console)
            val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnfGrammar.isDefined)
              clientLog("Parse Error:" + errstr)
            else {
              clientLog(checkAmbiguity(bnfGrammar.get))
            }

          case "normalize" =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed in the client console)
            val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnfGrammar.isDefined)
              clientLog("Parse Error:" + errstr)
            else {
              //convert the grammar to cnf form and then reconvert
              val normalization =
                (BNFConverter.ebnfToGrammar _
                  andThen CNFConverter.toCNF
                  andThen CNFConverter.cnfToGrammar
                  andThen CFGrammar.renameAutoSymbols)
              val normalGrammar = normalization(bnfGrammar.get)
              val data = Map("grammar" -> toJson(normalGrammar.toString))
              event("replace_grammar", data)
            }

          case "getHints" =>
            val pid = (msg \ "problemId").as[String].toInt
            val exercise = database1.grammarEntries.find(_.id == pid)
            exercise match {
              case None =>
                clientLog("There is no exercise with the given id")
              case Some(ex) =>
                val grammar = (msg \ "code").as[String]
                val rules = grammar.split("\n").toList
                //try to parse the grammar (syntax errors will be displayed in the console)
                val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
                if (!bnfGrammar.isDefined)
                  clientLog("Parse Error:" + errstr)
                else {
                  //create a future for the operation and add it to the futures list
                  val opctx = new OperationContext(enableExpensiveRepair = true, nOfTestcases = 100, nCorrectWordsForRepair = 100)
                  val hintFuture = Future {
                    provideHints(ex, bnfGrammar.get)(opctx)
                  }
                  recordFuture(hintFuture, opctx, None)
                  //disable hints and do-check, leave normalized untouched
                  disableEvents(List("getHints", "doCheck"))
                }
            }

          case "getHelp" =>
            val exid = (msg \ "exerciseId").as[String].toInt
            val extype = ExerciseType.getExType(exid)
            if (extype.isDefined) {
              val data = Map("message" -> toJson(helpMesage(extype.get)))
              event("helpmsg", data)
            } else
              //log error message
              clientLog("Exercise with id: " + exid + " does not exist")

          case "solve" =>
            if (!adminMode) {
              clientLog("'Solve' operation can be invoked only in admin mode.")
            } else {
              val pid = (msg \ "problemId").as[String].toInt
              database1.grammarEntries.find(_.id == pid) match {
                case None =>
                  clientLog("There is no grammar in the database with the selected id: " + pid)
                case Some(gentry) =>
                  val exid = (msg \ "exerciseId").as[String].toInt
                  val extype = ExerciseType.getExType(exid)
                  if (extype.isDefined) {
                    val userAnswer = (msg \ "code").as[String]
                    //get the solution for the exercise and dump it 
                    val sol = getSolution(gentry, extype.get, userAnswer)
                    if (sol.isDefined) {
                      val data = Map("solution" -> toJson(sol.get))
                      event("fullsolution", data)
                    } else
                      clientLog("Cannot solve the problem.")
                  } else
                    //log error message
                    clientLog("Exercise with id: " + exid + " does not exist")
              }
            }

          case _ =>
            clientError("Error: Unhandled client event " + msg)
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          clientError("Could not process event: " + t.getMessage)
      }

    case Quit =>
      //close the log file here
      eventLogger.close()

    case msg =>
      clientError("Unknown message: " + msg)(0)
  }

  import grammar.examples._
  import grammar.utils._
  import grammar._
  import repair._
  import parsing._
  import CFGrammar._
  import EBNFGrammar._
  import BNFConverter._
  import equivalence._
  import generators.LazyGenerator
  import grammar.exercises._
  import repair.RepairResult._
  import clients._
  import ExerciseType._

  type SententialForm = List[Symbol]
  type Word = List[Terminal]

  //minimum length of the word that has to be derived
  val minWordLength = 5
  val maxWordLength = 10
  //TODO: this is not thread safe, make this thread safe by extracting the string from
  //the problem statement
  var wordForDerivation: Option[Word] = None

  def generateProblemList(gentries: Seq[GrammarEntry]): Seq[(String, String)] = {
    //track the number of problems with the same name
    var seenNames = Set[String]()
    def getUniqueName(name: String, index: Int = 1): String = {
      val nname = if (index == 1)
        name
      else
        name + " " + index
      if (seenNames.contains(nname)) {
        getUniqueName(name, index + 1)
      } else nname
    }
    gentries.map(ge => {
      val newname = getUniqueName(ge.name)
      seenNames += newname
      (ge.id.toString -> newname)
    })
  }

  def helpMesage(ex: ExerciseType.ExType) = ex match {
    case DerivationEx =>
      "<ul><li>A leftmost derivation should have the start symbol on the first line</li>" +
        "<li>Each successive line, referred to as a step of the leftmost derivation, should be a sentential form " +
        "which is a sequence of nonterminals or terminals separated by whitespace. Terminals should not be enclosed within single quotes</li>" +
        "<li>Every step should be obtainable from the previous step by replacing the leftmost nonterminal by one of its productions</li>" +
        "<li>The last step of the derivation should be the string that has to be derived</li></ul>"

    case GrammarEx | CNFEx | GNFEx =>
      "<ul>Every line of the input should be a valid production in extended Backus-Naur form" +
        "<li> A production is of the form &lt;Nonterminal&gt; ::= &lt;Rightside&gt; (you can also use -> instead of ::= )</li>" +
        "<li>The left side of the first production is considered as the start symbol</li>" +
        "<li>Every symbol that does not appear on the left side of a production is considered a terminal</li>" +
        "<li>A &lt;Nonterminal&gt; is a sequence of alpha-numeric characters and underscore (_), that starts with an alphabet</li>" +
        "<li>A terminal is any contiguous sequence of characters, other than white-spaces and single quotes('), enclosed within single quotes." +
        " Single quotes can be omitted if the six reserved characters: (, ), *, + , ?, |, are not used by the terminal</li>" +
        "<li>\"\" denotes the empty string" +
        "<li> &lt;Rightside&gt; is a regular expression over terminals and nonterminals that uses | for disjunction, " +
        "* for closure, white-space for concatenation, parenthesis ( ) for grouping, + for closure without empty string, " +
        " and ? for option (disjunction with empty string)</li></ul>"
        
    case CYKEx => 
      "TODO"
  }

  def generateProblemStatement(gentry: GrammarEntry, extype: ExType): String = extype match {
    case GrammarEx if gentry.isLL1Entry =>
      "Provide an LL(1) grammar for " + gentry.desc
    case GrammarEx =>
      "Provide a grammar for " + gentry.desc
    case CNFEx =>
      "Convert the following grammar to Chomsky normal form " +
        gentry.reference.toHTMLString
    case GNFEx =>
      "Convert the following grammar to Griebach normal form " +
        gentry.reference.toHTMLString
    case DerivationEx =>
      //generate a word for derivation      
      wordForDerivation = (new LazyGenerator(gentry.cnfRef)(new OperationContext(
        maxIndexForGeneration = 500))).genRandomWord(minWordLength, maxWordLength)
      wordForDerivation match {
        case None =>
          "Cannot generate a word for the grammar of size: " + minWordLength
        case Some(w) =>
          "Provide a leftmost derivation for the word \"" + wordToString(w) +
            "\" from the grammar " + gentry.reference.toHTMLString
      }
    case CYKEx => 
      s"""Show the CYK parse table for the word "${wordToString(gentry.word.get)}" of the grammar """ + 
      	renameAutoSymbols(gentry.cnfRef).toHTMLString
  }

  def getSolution(gentry: GrammarEntry, extype: ExType, userAnswer: String): Option[String] = extype match {
    case GrammarEx | CNFEx | GNFEx | DerivationEx =>
      None
    case CYKEx =>
      //Note: here we are relying on the determinism of the renameAutoSymbols.
      //val g = renameAutoSymbols(gentry.cnfRef)
      //For now read the solution from the input/
      //TODO: fix this
      //here, we expect the user answer to be a grammar in EBNF form
      val rules = userAnswer.split("\n").toList
      val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
      if (!bnfGrammar.isDefined)
        Some("Parse Error:" + errstr)
      else {
        val g = bnfGrammar.get.cfGrammar
        val word = gentry.word.get
        //println("Word to parse: " + word)
        val parseWord = word.map(_.asInstanceOf[Terminal])
        val (_, cykTable) = (new CYKParser(g)).parseBottomUpWithTable(parseWord)(new OperationContext())
        //print every entry of the CYK table      
        val N = cykTable.length
        var str = ""
        for (k <- 1 to N) // substring length 
          for (p <- 0 to (N - k)) { // initial position 
            val i = p; val j = p + k - 1;
            str += s"""d($i)($j) = ${Util.setString(cykTable(i)(j).toSeq)} \n"""
          }
        Some(str)
      }
  }

  /**
   * Returns a string (which could be a temporary result) and also continuation
   * if more operation has to be performed
   */
  def checkSolution(gentry: GrammarEntry, extype: ExType, userAnswer: String)(implicit opctx: OperationContext): OpRes = {
    extype match {
      case GrammarEx =>
        //here, we expect the user answer to be a grammar in EBNF form
        val rules = userAnswer.split("\n").toList
        //try to parse the grammar (syntax errors will be displayed in the console)
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
        if (!bnfGrammar.isDefined)
          Last("Parse Error:" + errstr)
        else
          checkGrammarSolution(gentry, bnfGrammar.get)
      case DerivationEx =>
        //here, we expect the userAnswer to be a derivation
        //parse the input string into derivation steps        
        val (derivationSteps, errmsg) = (new SententialFormParser()).parseSententialForms(
          userAnswer.split("\n").toList, gentry.refGrammar)
        if (errmsg.isEmpty()) {
          //get the last word presented
          if (wordForDerivation.isDefined) {
            Last(checkDerivation(gentry, derivationSteps, wordForDerivation.get))
          } else
            Last("Cannot find the word to derive. Select the problem and try again!")
        } else
          Last("Parse Error: " + errmsg)
      case CNFEx =>
        //here, we expect the user answer to be a grammar in EBNF form                        
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(userAnswer.split("\n").toList)
        if (!bnfGrammar.isDefined)
          Last("Parse Error:" + errstr)
        else
          checkCNFSolution(gentry, bnfGrammar.get)

      case GNFEx =>
        //here, we expect the user answer to be a grammar in EBNF form                        
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(userAnswer.split("\n").toList)
        if (!bnfGrammar.isDefined)
          Last("Parse Error:" + errstr)
        else
          checkGNFSolution(gentry, bnfGrammar.get)
    }
  }

  def checkDerivation(gentry: GrammarEntry, derivationSteps: List[SententialForm], word: Word): String = {
    import DerivationChecker._

    DerivationChecker.checkLeftMostDerivation(word,
      derivationSteps, gentry.refGrammar) match {
        case Correct() =>
          "Correct."
        case InvalidStart() =>
          "Error: Derivation should start with: " + gentry.refGrammar.start
        case InvalidEnd() =>
          "Error: Derivation should end with: " + wordToString(word)
        case WrongStep(from, to, msg) =>
          "Error: cannot derive \"" + wordToString(to) + "\" form \"" + wordToString(from) + "\"" + ": " + msg
        case Other(msg) =>
          "Error: " + msg
      }
  }

  def checkCNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: OperationContext): OpRes = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      Last("The grammar is in EBNF form. You cannot use *,+,? in CNF form")
    } else {
      val g = ebnfToGrammar(studentGrammar)
      CFGrammar.getRuleNotInCNF(g, false) match {
        case None =>
          Partial("The grammar staisfies CNF properties.\nchecking for equivalence...",
            Future { checkEquivalence(gentry.cnfRef, g) })
        case Some(Error(rule, msg)) =>
          Last("Rule not in CNF: " + rule + " : " + msg)
      }
    }
  }

  def checkGNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: OperationContext): OpRes = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      Last("The grammar is in EBNF form. You cannot use *,+,? in GNF form")
    } else {
      val g = ebnfToGrammar(studentGrammar)
      CFGrammar.getRuleNotInGNF(g) match {
        case None =>
          Partial("The grammar staisfies GNF properties.\nchecking for equivalence...",
            Future { checkEquivalence(gentry.cnfRef, g) })
        case Some(Error(rule, msg)) =>
          Last("Rule not in Greibach Normal Form: " + rule + " : " + msg)
      }
    }
  }

  def ll1FeedbackStr(studentGrammar: BNFGrammar): (Boolean, String) = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      (false, "Regular expression operations *,+,? are not supported by the LL1 check." +
        "Remove them and retry")
    } else {
      GrammarUtils.isLL1WithFeedback(studentGrammar.cfGrammar) match {
        case fb @ GrammarUtils.InLL1() => (true, fb.toString)
        case ll1feedback =>
          (false, ll1feedback.toString)
      }
    }
  }

  def checkLL1(studentGrammar: BNFGrammar): Option[String] = {
    val ll1checkRes = ll1FeedbackStr(studentGrammar)._2
    Some(ll1checkRes + "\n" + (if (this.adminMode) {
      //if in admin mode include first/follow sets
      val (nullables, first, follow) = GrammarUtils.nullableFirstFollow(studentGrammar.cfGrammar)
      "Nullables: " + Util.setString(nullables.toSeq) + "\n" + first.collect {
        case (k: Nonterminal, v) =>
          s"""first($k) -> ${Util.setString(v.toSeq)}"""
      }.mkString("\n") + "\n" + follow.collect {
        case (k: Nonterminal, v) => s"""follow($k) -> ${Util.setString(v.toSeq)}"""
      }.mkString("\n")
    } else
      ""))
  }

  import clients.AmbiguityChecker._
  def checkAmbiguity(studentGrammar: BNFGrammar): String = {
    checkForAmbiguity(studentGrammar.cfGrammar)(new OperationContext()) match {
      case Unambiguous() =>
        "The grammar is unambiguous."
      case PossiblyUnambiguous() =>
        "The grammar is possibly unambiguous."
      case AmbiguousString(w) =>
        "There are at least two parse trees for: " + wordToString(w)
    }
  }

  def checkGrammarSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: OperationContext): OpRes = {

    //check if the exercise requires the grammar to be in LL1
    val ll1feedback = if (gentry.isLL1Entry) {
      //check for LL1
      val ll1res = ll1FeedbackStr(studentGrammar)
      if (!ll1res._1) {
        "Warning: LL(1) check failed: " + ll1res._2 + "\n\n"
      } else
        ""
    } else ""
    checkEquivalence(gentry.cnfRef, studentGrammar.cfGrammar) match {
      case Last(resstr) =>
        Last(ll1feedback + resstr)
      case Partial(resstr, nextPart) => Partial(ll1feedback + resstr, nextPart)
    }

  }

  def checkEquivalence(ref: Grammar, g: Grammar)(implicit opctx: OperationContext): OpRes = {

    def proveEquivalence(g1: Grammar, g2: Grammar): Boolean = {
      val verifier = new EquivalenceVerifier(g1, g2)
      verifier.proveEquivalence() match {
        case Some(true) => true
        case _ => false
      }
    }

    val cnfG = CNFConverter.toCNF(g)
    if (cnfG.rules.isEmpty) {
      Last("The grammar accepts/produces no strings! Check if all rules are reachable and productive !")
    } else {
      val equivChecker = new EquivalenceChecker(ref)
      equivChecker.isEquivalentTo(cnfG) match {
        case PossiblyEquivalent => {
          //print a temporary status message here here, iff this has not been aborted          
          Partial("All testcases passed.\nProving equivalence ... ",
            Future {
              Last(if (proveEquivalence(equivChecker.cnfRef, cnfG))
                "Correct."
              else
                "Possibly correct but unable to prove correctness.")
            })
        }
        case InadequateTestcases =>
          Last("Cannot generate enough testcases to prove correctness.\n" +
            "Make the grammar less ambiguous and try again!")
        case NotEquivalentNotAcceptedBySolution(ex) =>
          Last("The grammar does not accept the string: " + wordToString(ex))

        case NotEquivalentNotGeneratedBySolution(ex) =>
          Last("The grammar accepts the invalid string: " + wordToString(ex))
        case Aborted =>
          Last("Aborted")
      }
    }
  }

  def provideHints(ex: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: OperationContext): OpRes = {

    val maxHintsSize = 5

    def prettyPrintFeedbacks(inG: Grammar, resG: Grammar, w: Word, feedbacks: List[GrammarFeedback]) = {
      //pretty print feedbacks by renaming new non-terminals to simpler names
      val newnonterms = resG.nonTerminals.filterNot(CNFConverter.isCNFNonterminal _).toSet --
        nonterminals(inG)
      val renameMap = genRenameMap(newnonterms.toList, nonterminals(resG))
      feedbacks.map(f => {
        val feedbackString = f match {
          case AddAllRules(rules) =>
            var msg = "To accept the string: " + wordToString(w) + "\n"
            val newMap = renameMap.map {
              //add question mark to the names of the nonterminals
              case (k, Nonterminal(n)) => (k, Nonterminal(n + "?"))
            }
            msg += "Try adding: " + rulesToStr(replace(rules, newMap))
            if (!newnonterms.isEmpty) {
              //println("newnontemrs: " + newnonterms)
              msg + "\nNonterminals with '?' could belong to the grammar or could be a fresh nonterminal"
            } else
              msg

          case RemoveRules(rules, None) =>
            "To block the string: " + wordToString(w) +
              "\nRemove: " + rulesToStr(rules)

          case RemoveRules(rules, Some(repairRules)) =>
            "To block the string: \"" + wordToString(w) + "\"" +
              "\nRemove the rule: " + rulesToStr(rules) +
              " that results in the derivation of the invalid string"

          case RefineRules(olds, news, repairRules) =>
            "To block the string: \"" + wordToString(w) + "\"" +
              " the derivation: " + rulesToStr(repairRules) +
              " should be blocked in context: " + rulesToStr(olds) +
              "\nTry replacing: " + rulesToStr(olds) +
              "\nby: " + rulesToStr(replace(news, renameMap))

          case ExpandRules(olds, news) =>
            "Grammar generates invalid string: \"" + wordToString(w) + "\"" +
              "\nTry hints after expanding the righside of the rule: " + rulesToStr(olds) +
              "\nby inlining the productions of the nonterminals in the rightside: " +
              "\nEg. as " + (if (news.size <= maxHintsSize)
                rulesToStr(replace(news, renameMap))
              else
                rulesToStr(replace(news.take(5), renameMap)) + " ...")

          case NoRepair(reason) =>
            "Cannot provide hints: " + reason
        }
        feedbackString
      }).mkString("\n")
    }

    val resstr = if (BNFConverter.usesRegOp(studentGrammar)) {
      "The grammar is in EBNF form. Click on \"Normalize\" and retry."
    } else {
      val plainGrammar = ebnfToGrammar(studentGrammar)
      if (!CFGrammar.isNormalized(plainGrammar)) {
        "Some rules are not normalized. Click on \"Normalize\" and retry."
      } else {

        val cnfG = CNFConverter.toCNF(plainGrammar)
        if (cnfG.rules.isEmpty) {
          "The grammar is empty. Not all rules are produtive and reachable."
        } else {
          val equivChecker = new EquivalenceChecker(ex.cnfRef)
          val repairer = new Repairer(equivChecker)
          equivChecker.isEquivalentTo(cnfG) match {
            case equivResult @ PossiblyEquivalent =>
              "The grammar is probably correct. Try checking this grammar."
            case InadequateTestcases =>
              "Cannot generate enough testcases to perform repair.\n" +
                "Make the grammar less ambiguous and try again!"
            case equivResult @ NotEquivalentNotAcceptedBySolution(ex) =>
              val (resG, feedbacks) = repairer.hint(cnfG, equivResult)
              prettyPrintFeedbacks(plainGrammar, resG, ex, feedbacks)

            case equivResult @ NotEquivalentNotGeneratedBySolution(ex) =>
              val (resG, feedbacks) = repairer.hint(cnfG, equivResult)
              prettyPrintFeedbacks(plainGrammar, resG, ex, feedbacks)
            case Aborted =>
              "Aborted"
          }
        }
      }
    }
    Last(resstr)
  }
}

