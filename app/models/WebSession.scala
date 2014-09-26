package models

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.Future
import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._
import akka.pattern.ask
import play.api.Play.current
import grammar.GrammarParser
import grammar.exercises.Exercise
import grammar.EBNFGrammar.BNFGrammar
import grammar.CFGrammar.Grammar
import grammar.BNFConverter
import parsing.CNFConverter
import grammar.CFGrammar
import grammar.exercises.LL1Exercise
import grammar.exercises.ExerciseSet

object ExerciseSet1 extends ExerciseSet(Play.getFile("/public/resources/ExerciseSet1.xml")) {  
}

class WebSession(remoteIP: String) extends Actor {
  import Protocol._
  
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

          case "doUpdateCode" =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed to the console)
            //TODO: prevent user grammars from using hypens in the names of symbols
            //it is reserved for newly created symbols
            val (bnf, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnf.isDefined)
              clientLog("Parse Error: " + errstr)

          case "getExerciseList" =>
            //read all exercises in the grammar package and send their names and ids to the clients
            val exercises = ExerciseSet1.exercises 
            val data = exercises.map(ex => (ex.id.toString -> toJson(ex.name))).toMap
            //val data = Map("ex1" -> toJson("Exercise 1"))
            event("exercises", data)

          case "loadExercise" =>
            val exid = (msg \ "exerciseId").as[String].toInt
            val exercise = ExerciseSet1.exercises.find(_.id == exid)
            val data = exercise match {
              case None =>
                Map("desc" -> toJson("There is no exercise with the given id"))
              case Some(ex) =>
                Map("desc" -> toJson(ex.desc))
            }
            event("exerciseDesc", data)

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

          case "doCheck" =>
            val exid = (msg \ "exerciseId").as[String].toInt
            val exercise = ExerciseSet1.exercises.find(_.id == exid)
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
                else
                  checkSolution(ex, bnfGrammar.get)
            }
          case "getHints" =>
            val exid = (msg \ "exerciseId").as[String].toInt
            val exercise = ExerciseSet1.exercises.find(_.id == exid)
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
                else
                  provideHints(ex, bnfGrammar.get)
            }

          case _ =>
            clientError("Error: Unhandled client event " + msg)
        }
      } catch {
        case t: Throwable =>
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
  import grammar.exercises.ExerciseSet
  import grammar.exercises.Exercise
  import repair.RepairResult._

  def checkSolution(ex: Exercise, studentGrammar: BNFGrammar) {

    val nOfTests = 100
    val debug = false

    clientLog("===============")
    /*println(ex.id + ": " + ex.name)
    println(ex.desc)*/
    //println("Evaluating solution ")

    val plainGrammar = ebnfToGrammar(studentGrammar)
    //check if the exercise requires the grammar to be in LL1
    val ll1ok = ex match {
      case _: LL1Exercise =>
        //check for LL1
        GrammarUtils.isLL1WithFeedback(plainGrammar) match {
          case GrammarUtils.InLL1() => 
            true
          case ll1feedback =>
            clientLog("LL(1) check failed.") 
            clientLog(ll1feedback.toString)
            false
        }
      case _ => true
    }
    if (ll1ok) {
      val cnfG = CNFConverter.toCNF(plainGrammar)
      if (cnfG.rules.isEmpty) {
        clientLog("The grammar accepts/produces no strings! Check if all rules are reachable and productive !")
      } else {

        if (debug) {
          clientLog("Your Grammar after normalization and epsilon removal: " + CNFConverter.cnfToGrammar(cnfG))
          println("Plain Student's Grammar: " + plainGrammar)
          println("Strings for student grammar: " + (new LazyGenerator(cnfG)).getIterator(30).map(_.mkString(" ")).mkString("\n"))
          //System.exit(0)
        }

        val cfg = ebnfToGrammar(ex.reference)
        val equivChecker = new EquivalenceChecker(cfg, nOfTests)
        val repairer = new Repairer(equivChecker)
        if (debug) {
          /*println("Reference Grammar: ")
    		println(ex.reference)*/
          //println("Reference Grammar In GNF: " + GNFConverter.toGNF(equivChecker.cnfRef))      
          println("Tests: " + equivChecker.words.take(30).map(_.mkString(" ")).mkString("\n"))
        }

        //println("Student Grammar In GNF: " + GNFConverter.toGNF(cnfG))
        //proveEquivalence(equivChecker.cnfRef, cnfG)

        equivChecker.isEquivalentTo(cnfG) match {
          case equivResult @ PossiblyEquivalent() => {
            if (proveEquivalence(equivChecker.cnfRef, cnfG)) {
              clientLog("Correct.")
            } else
              clientLog("Possibly correct but unable prove correctness.")
          }
          case equivResult @ NotEquivalentNotAcceptedBySolution(ex) =>
            clientLog("The grammar does not accept the string: " + wordToString(ex))

          case equivResult @ NotEquivalentNotGeneratedBySolution(ex) =>
            clientLog("The grammar accepts the invalid string: " + wordToString(ex))
        }
        //clientLog(GrammarUtils.isLL1WithFeedback(plainGrammar).map("LL1:     " + _).getOrElse("LL1:     OK"))
      }
    }
    clientLog("===============")
    /*//for stats    
    val pr = new java.io.PrintWriter(quiz.quizName + "-stats.txt")
    Stats.dumpStats(pr)
    pr.close()*/

    def proveEquivalence(g1: Grammar, g2: Grammar): Boolean = {
      val verifier = new EquivalenceVerifier(g1, g2, nOfTests)
      verifier.proveEquivalence() match {
        case Some(true) => true
        case _ => false
      }
    }
  }

  def provideHints(ex: Exercise, studentGrammar: BNFGrammar) {

    val nOfTests = 100
    val debug = false

    clientLog("===============")

    if (BNFConverter.usesRegOp(studentGrammar)) {
      clientLog("The grammar is in EBNF form. Normalize the grammar.")
    } else {
      val plainGrammar = ebnfToGrammar(studentGrammar)
      if (!CFGrammar.isNormalized(plainGrammar)) {
        clientLog("Some rules are not normalized. Normalize the grammar.")
      } else {

        val cnfG = CNFConverter.toCNF(plainGrammar)
        if (cnfG.rules.isEmpty) {
          clientLog("The grammar is empty. Not all rules are produtive and reachable.")
        } else {

          if (debug) {
            clientLog("Your Grammar after normalization and epsilon removal: " + CNFConverter.cnfToGrammar(cnfG))
            println("Plain Student's Grammar: " + plainGrammar)
            println("Strings for student grammar: " + (new LazyGenerator(cnfG)).getIterator(30).map(_.mkString(" ")).mkString("\n"))
            //System.exit(0)
          }

          val cfg = ebnfToGrammar(ex.reference)
          val equivChecker = new EquivalenceChecker(cfg, nOfTests)
          val repairer = new Repairer(equivChecker)
          if (debug) {
            /*println("Reference Grammar: ")
    		println(ex.reference)*/
            //println("Reference Grammar In GNF: " + GNFConverter.toGNF(equivChecker.cnfRef))      
            println("Tests: " + equivChecker.words.take(30).map(_.mkString(" ")).mkString("\n"))
          }

          //println("Student Grammar In GNF: " + GNFConverter.toGNF(cnfG))
          //proveEquivalence(equivChecker.cnfRef, cnfG)

          equivChecker.isEquivalentTo(cnfG) match {
            case equivResult @ PossiblyEquivalent() => {
              clientLog("The grammar is probably correct. Try checking this grammar.")
            }
            case equivResult @ NotEquivalentNotAcceptedBySolution(ex) =>
              clientLog("To accept the string: " + wordToString(ex))
              val (resG, feedbacks) = repairer.hint(cnfG, equivResult)
              prettyPrintFeedbacks(cnfG, resG, feedbacks)

            case equivResult @ NotEquivalentNotGeneratedBySolution(ex) =>
              clientLog("To block the string: " + wordToString(ex))
              val (resG, feedbacks) = repairer.hint(cnfG, equivResult)
              prettyPrintFeedbacks(cnfG, resG, feedbacks)
          }
        }
      }
    }
    clientLog("===============")
    /*//for stats    
    val pr = new java.io.PrintWriter(quiz.quizName + "-stats.txt")
    Stats.dumpStats(pr)
    pr.close()*/

    def prettyPrintFeedbacks(cnfG: Grammar, resG: Grammar, feedbacks: List[GrammarFeedback]) = {
      //pretty print feedbacks by renaming new non-terminals to simpler names
      val newnonterms = nonterminals(resG) -- nonterminals(cnfG)
      val renameMap = genRenameMap(newnonterms, nonterminals(resG))
      feedbacks.foreach(f => {
        val feedbackString = f match {
          case AddAllRules(rules) =>
            clientLog("Add: " + replace(rules, renameMap).mkString("\n"))
          case RemoveRules(rules) =>
            clientLog("Remove: " + replace(rules, renameMap).mkString("\n"))
          case RefineRules(olds, news) =>
            clientLog("Replace: " + olds.mkString("\n"))
            clientLog("by: " + replace(news, renameMap).mkString("\n"))
          case ExpandRules(olds, news) =>
            clientLog("First, expand the righside of the rule: " + olds.mkString("\n"))
            clientLog("as: " + replace(news, renameMap).mkString("\n"))
        }
      })
    }
  }

}

