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

object database1 extends GrammarDatabase(Play.getFile("/public/resources/GrammarDatabase.xml")) {
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

  def clientLog(msg: String) = {
    eventLogger.println("[>] L: " + msg)
    eventLogger.flush()
    pushMessage(toJson(Map("kind" -> "console", "level" -> "log", "message" -> msg)))
  }

  def clientError(msg: String) = {
    eventLogger.println("[>] E: " + msg)
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
  var currentOp: Option[Future[OpRes]] = None
  def recordFuture(opfuture: Future[OpRes], extype: Option[ExerciseType.ExType]) {
    currentOp = Some(opfuture)
    //register a call-back
    opfuture onComplete {
      case Success(opRes) if (currentOp == Some(opfuture)) =>
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
            recordFuture(nextPart, extype)
        }
      //else do nothing            
      case Failure(msg) if (currentOp == Some(opfuture)) =>
        clientLog("Operation aborted abnormally")
        //print the exception to the console
        println(msg)

      case _ =>
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

  def receive = {
    case Init =>
      sender ! InitSuccess(enumerator)
    //clientLog("New client")

    case FromClient(msg) =>
      try {
        eventLogger.println("[<] " + msg)
        eventLogger.flush()

        (msg \ "action").as[String] match {
          case "hello" =>
            clientLog("Welcome!")

          case "abortOps" =>
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
                  val checkFuture = Future {
                    checkSolution(gentry, extype.get, userAnswer)
                  }
                  recordFuture(checkFuture, extype)
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
              val ll1feedback = checkLL1(bnfGrammar.get)
              if (ll1feedback.isEmpty)
                clientLog("The grammar is in LL(1)")
              else
                clientLog(ll1feedback.get)
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
                  val hintFuture = Future {
                    provideHints(ex, bnfGrammar.get)
                  }
                  recordFuture(hintFuture, None)
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
      clientError("Unknown message: " + msg)
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
  val maxIndexToExplore = 500
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
        "which is a sequence of nonterminals or terminals separated by whitespace</li>" +
        "<li>Every step should be obtainable from the previous step by replacing the leftmost nonterminal by one of its productions</li>" +
        "<li>The last step of the derivation should be the string required to be derived</li></ul>"

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
  }

  def generateProblemStatement(gentry: GrammarEntry, extype: ExType): String = extype match {
    case GrammarEx if gentry.isLL1Entry =>
      "Provide a LL(1) grammar for " + gentry.desc
    case GrammarEx =>
      "Provide a grammar for " + gentry.desc
    case CNFEx =>
      "Convert the following grammar to CNF normal form " +
        gentry.reference.toHTMLString
    case GNFEx =>
      "Convert the following grammar to GNF normal form " +
        gentry.reference.toHTMLString
    case DerivationEx =>
      //generate a word for derivation      
      wordForDerivation = (new LazyGenerator(gentry.cnfRef)).genRandomWord(minWordLength,
        maxWordLength, maxIndexToExplore)
      wordForDerivation match {
        case None =>
          "Cannot generate a word for the grammar of size: " + minWordLength
        case Some(w) =>
          "Provide a leftmost derivation for the word \"" + wordToString(w) +
            "\" from the grammar " + gentry.reference.toHTMLString
      }
  }

  /**
   * Returns a string (which could be a temporary result) and also continuation
   * if more operation has to be performed
   */
  def checkSolution(gentry: GrammarEntry, extype: ExType, userAnswer: String): OpRes = {
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
        val (derivationSteps, errmsg) = (new GrammarParser()).parseSententialForms(
          userAnswer.split("\n").toList, gentry.refGrammar)
        if (derivationSteps.isDefined) {
          //get the last word presented
          if (wordForDerivation.isDefined) {
            Last(checkDerivation(gentry, derivationSteps.get, wordForDerivation.get))
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

  def checkCNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar): OpRes = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      Last("The grammar is in EBNF form. You cannot use *,+,? in CNF form")
    } else {
      val g = ebnfToGrammar(studentGrammar)
      CFGrammar.getRuleNotInCNF(g) match {
        case None =>
          Partial("The grammar staisfies CNF properties.\nchecking for equivalence...",
            Future { checkEquivalence(gentry.cnfRef, g) })
        case Some(Error(rule, msg)) =>
          Last("Rule not in CNF: " + rule + " : " + msg)
      }
    }
  }

  def checkGNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar): OpRes = {
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

  def checkLL1(studentGrammar: BNFGrammar): Option[String] = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      Some("Regular expression operations *,+,? are not supported by the LL1 check." +
        "Remove them and retry")
    } else {
      GrammarUtils.isLL1WithFeedback(studentGrammar.cfGrammar) match {
        case GrammarUtils.InLL1() => None
        case ll1feedback =>
          Some(ll1feedback.toString)
      }
    }
  }

  import clients.AmbiguityChecker._
  def checkAmbiguity(studentGrammar: BNFGrammar): String = {
    checkForAmbiguity(studentGrammar.cfGrammar) match {
      case Unambiguous() =>
        "The grammar is unambiguous."
      case PossiblyUnambiguous() =>
        "The grammar is possibly unambiguous."
      case AmbiguousString(w) =>
        "The are at least two parse trees for: " + wordToString(w)
    }
  }

  def checkGrammarSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar): OpRes = {

    //check if the exercise requires the grammar to be in LL1
    val ll1feedback = if (gentry.isLL1Entry) {
      //check for LL1
      val ll1res = checkLL1(studentGrammar)
      if (ll1res.isDefined) {
        "Warning: LL(1) check failed: " + ll1res.get + "\n\n"
      } else
        ""
    } else ""
    checkEquivalence(gentry.cnfRef, studentGrammar.cfGrammar) match {
      case Last(resstr) =>
        Last(ll1feedback + resstr)
      case Partial(resstr, nextPart) => Partial(ll1feedback + resstr, nextPart)
    }

  }

  def checkEquivalence(ref: Grammar, g: Grammar): OpRes = {
    val nOfTests = 100
    val debug = false

    def proveEquivalence(g1: Grammar, g2: Grammar): Boolean = {
      val verifier = new EquivalenceVerifier(g1, g2, nOfTests)
      verifier.proveEquivalence() match {
        case Some(true) => true
        case _ => false
      }
    }

    val cnfG = CNFConverter.toCNF(g)
    if (cnfG.rules.isEmpty) {
      Last("The grammar accepts/produces no strings! Check if all rules are reachable and productive !")
    } else {
      if (debug) {
        clientLog("Grammar in CNF: " + cnfG)
      }
      val equivChecker = new EquivalenceChecker(ref, nOfTests)
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
      }
    }
  }

  def provideHints(ex: GrammarEntry, studentGrammar: BNFGrammar): OpRes = {

    val debug = false
    val nOfTests = 100
    val maxHintsSize = 5

    def prettyPrintFeedbacks(inG: Grammar, resG: Grammar, w: Word, feedbacks: List[GrammarFeedback]) = {
      //pretty print feedbacks by renaming new non-terminals to simpler names
      val newnonterms = resG.nonTerminals.filterNot(CNFConverter.isCNFNonterminal _).toSet --
        nonterminals(inG)
      val renameMap = genRenameMap(newnonterms, nonterminals(resG))
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
              msg + "\nNonterminals with '?' can belong to the grammar or could be a fresh nonterminal"
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

          case NoRepair =>
            "Cannot provide hints."
        }
        feedbackString
      }).mkString("\n")
    }

    val resstr = if (BNFConverter.usesRegOp(studentGrammar)) {
      "The grammar is in EBNF form. Normalize the grammar."
    } else {
      val plainGrammar = ebnfToGrammar(studentGrammar)
      if (!CFGrammar.isNormalized(plainGrammar)) {
        "Some rules are not normalized. Normalize the grammar."
      } else {

        val cnfG = CNFConverter.toCNF(plainGrammar)
        if (cnfG.rules.isEmpty) {
          "The grammar is empty. Not all rules are produtive and reachable."
        } else {
          val equivChecker = new EquivalenceChecker(ex.cnfRef, nOfTests)
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
          }
        }
      }
    }
    Last(resstr)
  }
}

