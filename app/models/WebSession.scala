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

object database1 extends GrammarDatabase(Play.getFile("/public/resources/GrammarDatabase.xml")) {
}

class WebSession(remoteIP: String) extends Actor {
  import Protocol._
  //creating a thread pool for futures
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

  val (enumerator, channel) = Concurrent.broadcast[JsValue]

  def pushMessage(v: JsValue) = channel.push(v)

  def clientLog(msg: String) = {
    Logger.info("[>] L: " + msg)
    pushMessage(toJson(Map("kind" -> "console", "level" -> "log", "message" -> msg)))
  }

  def clientError(msg: String) = {
    Logger.info("[>] E: " + msg)
    pushMessage(toJson(Map("kind" -> "console", "level" -> "error", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    Logger.info("[>] " + kind)
    pushMessage(toJson(Map("kind" -> toJson(kind)) ++ data))
  }

  //a list operation futures
  var currentOp: Option[Future[String]] = None
  def recordFuture(opfuture: Future[String], extype : Option[ExerciseType.ExType]) {
    currentOp = Some(opfuture)
    //register a call-back
    opfuture onComplete {
      case Success(resstr) =>
        //check if the future has been super-seeded other operations,
        //in which case ignore the result
        if (currentOp == Some(opfuture)){
          clientLog(resstr)          
          extype match {
            case Some(ExerciseType.GrammarEx) =>
              enableEvents(List("doCheck","getHints"))
            case Some(_) => 
              enableEvents(List("doCheck"))
            case _ => 
              //allow every thing here
              enableEvents(List("doCheck","getHints"))
          }                     
        }
      //else do nothing            
      case Failure(msg) =>
        if (currentOp == Some(opfuture)) {
          clientLog("Operation aborted abnormally")
          //print the exception to the console
          println(msg)
        }
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
        Logger.info("[<] " + msg)

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
            //create logs in this case
            ;
          /*val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
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
                  andThen CFGrammar.simplifyGrammar
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

          case _ =>
            clientError("Error: Unhandled client event " + msg)
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          clientError("Could not process event: " + t.getMessage)
      }

    case Quit =>

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

  def checkSolution(gentry: GrammarEntry, extype: ExType, userAnswer: String): String = {

    extype match {
      case GrammarEx =>
        //here, we expect the user answer to be a grammar in EBNF form
        val rules = userAnswer.split("\n").toList
        //try to parse the grammar (syntax errors will be displayed in the console)
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
        if (!bnfGrammar.isDefined)
          "Parse Error:" + errstr
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
            checkDerivation(gentry, derivationSteps.get, wordForDerivation.get)
          } else
            "Cannot find the word to derive. Select the problem and try again!"
        } else
          "Parse Error: " + errmsg
      case CNFEx =>
        //here, we expect the user answer to be a grammar in EBNF form                        
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(userAnswer.split("\n").toList)
        if (!bnfGrammar.isDefined)
          "Parse Error:" + errstr
        else
          checkCNFSolution(gentry, bnfGrammar.get)

      case GNFEx =>
        //here, we expect the user answer to be a grammar in EBNF form                        
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(userAnswer.split("\n").toList)
        if (!bnfGrammar.isDefined)
          "Parse Error:" + errstr
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

  def checkCNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar): String = {
    val g = ebnfToGrammar(studentGrammar)
    CFGrammar.getRuleNotInCNF(g) match {
      case None =>
        checkEquivalence(gentry.cnfRef, g)
      case Some(Error(rule, msg)) =>
        "Rule not in CNF: " + rule + " : " + msg
    }
  }

  def checkGNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar): String = {
    val g = ebnfToGrammar(studentGrammar)
    CFGrammar.getRuleNotInGNF(g) match {
      case None =>
        checkEquivalence(gentry.cnfRef, g)
      case Some(Error(rule, msg)) =>
        "Rule not in Greibach Normal Form: " + rule + " : " + msg
    }
  }

  def checkGrammarSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar): String = {

    val g = ebnfToGrammar(studentGrammar)
    //check if the exercise requires the grammar to be in LL1
    val ll1ok = if (gentry.isLL1Entry) {
      //check for LL1
      GrammarUtils.isLL1WithFeedback(g) match {
        case GrammarUtils.InLL1() =>
          true
        case ll1feedback =>
          clientLog("Warning: LL(1) check failed.")
          clientLog(ll1feedback.toString)
          false
      }
    } else true
    //if (ll1ok) {
    checkEquivalence(gentry.cnfRef, g)
    //}
    /*//for stats, remember stats    
    val pr = new java.io.PrintWriter(quiz.quizName + "-stats.txt")
    Stats.dumpStats(pr)
    pr.close()*/
  }

  def checkEquivalence(ref: Grammar, g: Grammar): String = {
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
      "The grammar accepts/produces no strings! Check if all rules are reachable and productive !"
    } else {
      if (debug) {
        clientLog("Grammar in CNF: " + cnfG)
      }
      val equivChecker = new EquivalenceChecker(ref, nOfTests)
      equivChecker.isEquivalentTo(cnfG) match {
        case PossiblyEquivalent => {
          if (proveEquivalence(equivChecker.cnfRef, cnfG)) {
            "Correct."
          } else
            "Possibly correct but unable to prove correctness."
        }
        case InadequateTestcases =>
          "Cannot generate enough testcases to prove correctness.\n" +
            "Make the grammar less ambiguous and try again!"
        case NotEquivalentNotAcceptedBySolution(ex) =>
          "The grammar does not accept the string: " + wordToString(ex)

        case NotEquivalentNotGeneratedBySolution(ex) =>
          "The grammar accepts the invalid string: " + wordToString(ex)
      }
    }
  }

  def provideHints(ex: GrammarEntry, studentGrammar: BNFGrammar): String = {

    val debug = false
    val nOfTests = 100

    def prettyPrintFeedbacks(inG: Grammar, resG: Grammar, feedbacks: List[GrammarFeedback]) = {
      //pretty print feedbacks by renaming new non-terminals to simpler names
      val newnonterms = nonterminals(resG) -- nonterminals(inG)
      val renameMap = genRenameMap(newnonterms, nonterminals(resG))
      feedbacks.map(f => {
        val feedbackString = f match {
          case AddAllRules(rules) =>
            "Add: " + replace(rules, renameMap).mkString("\n")
          case RemoveRules(rules) =>
            "Remove: " + replace(rules, renameMap).mkString("\n")
          case RefineRules(olds, news) =>
            "Replace: " + olds.mkString("", "\n", "\n") +
              "by: " + replace(news, renameMap).mkString("\n")
          case ExpandRules(olds, news) =>
            "First, expand the righside of the rule: " + olds.mkString("", "\n", "\n") +
              "as: " + replace(news, renameMap).mkString("\n")
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
              "To accept the string: " + wordToString(ex) + "\n" +
                prettyPrintFeedbacks(plainGrammar, resG, feedbacks)

            case equivResult @ NotEquivalentNotGeneratedBySolution(ex) =>
              val (resG, feedbacks) = repairer.hint(cnfG, equivResult)
              "To block the string: " + wordToString(ex) + "\n" +
                prettyPrintFeedbacks(plainGrammar, resG, feedbacks)
          }
        }
      }
    }
    resstr
    /*//for stats    
    val pr = new java.io.PrintWriter(quiz.quizName + "-stats.txt")
    Stats.dumpStats(pr)
    pr.close()*/
  }
}

