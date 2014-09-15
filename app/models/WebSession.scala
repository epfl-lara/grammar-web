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

class WebSession(remoteIP: String) extends Actor {
  import Protocol._

  val (enumerator, channel) = Concurrent.broadcast[JsValue]

  def pushMessage(v: JsValue) = channel.push(v)

  def clientLog(msg: String) = {
    Logger.info("[>] L: "+msg)
    pushMessage(toJson(Map("kind" -> "console", "level" -> "log", "message" -> msg)))
  }

  def clientError(msg: String) = {
    Logger.info("[>] E: "+msg)
    pushMessage(toJson(Map("kind" -> "console", "level" -> "error", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    Logger.info("[>] "+kind)
    pushMessage(toJson(Map("kind" -> toJson(kind)) ++ data))
  }

  def receive = {
    case Init =>
      sender ! InitSuccess(enumerator)

      clientLog("New client")

    case FromClient(msg) =>
      try {
        Logger.info("[<] "+msg)

        (msg \ "action").as[String] match {
          case "hello" =>
            clientLog("Welcome!")

          case "doUpdateCode" =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList            
            //try to parse the grammar (syntax errors will be displayed to the console)
            val bnfGrammar = (new GrammarParser()).parseGrammar(rules)
            clientLog("Your Grammar: "+bnfGrammar)
            
          case "getExerciseList" => 
            //read all exercises in the grammar package and send their names and ids to the clients
            val exercises = grammar.exercises.ExerciseSet1
            val data = exercises.get.map(ex => (ex.id.toString -> toJson(ex.name))).toMap 
            //val data = Map("ex1" -> toJson("Exercise 1"))
            event("exercises", data)
          
          case "loadExercise" => 
            val exid = (msg \ "exerciseId").as[String].toInt
            val exercise = grammar.exercises.ExerciseSet1.get.find(_.id  == exid)
            val data = exercise match {
              case None =>
                Map("desc" -> toJson("There is no exercise with the given id"))
              case Some(ex) =>
                Map("desc" -> toJson(ex.desc))
            }
            event("exerciseDesc",data)
          
          case "doCheck" => 
            val exid = (msg \ "exerciseId").as[String].toInt
            val exercise = grammar.exercises.ExerciseSet1.get.find(_.id  == exid)
            exercise match {
              case None =>
                clientLog("There is no exercise with the given id")                
              case Some(ex) =>
                val grammar = (msg \ "code").as[String]
                val rules = grammar.split("\n").toList            
                //try to parse the grammar (syntax errors will be displayed to the console)
                val bnfGrammar = (new GrammarParser()).parseGrammar(rules)
                checkSolution(ex, bnfGrammar)
            }            
            
          case _ =>
            clientError("Error: Unhandled client event "+msg)
        }
      } catch {
        case t: Throwable =>
          clientError("Could not process event: "+t.getMessage)
      }

    case Quit =>

    case msg =>
      clientError("Unknown message: "+msg)
  }
  
  def checkSolution(ex: Exercise, studentGrammar : BNFGrammar) {

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
    
    val nOfTests = 100
    val debug = false

    clientLog("===============")
    /*println(ex.id + ": " + ex.name)
    println(ex.desc)*/    
    //println("Evaluating solution ")

    val cfg = ebnfToGrammar(ex.reference)
    val equivChecker = new EquivalenceChecker(cfg, nOfTests)
    val repairer = new Repairer(equivChecker)

    /*println("Reference Grammar: ")
    println(ex.reference)*/

    //println("Reference Grammar In GNF: " + GNFConverter.toGNF(equivChecker.cnfRef))
    if (debug) {
      println("Tests: " + equivChecker.words.take(30).map(_.mkString(" ")).mkString("\n"))
    }
    val plainGrammar = ebnfToGrammar(studentGrammar)
    val cnfG = CNFConverter.toCNF(plainGrammar)

    if (debug && !cnfG.rules.isEmpty) {
      println("Plain Student's Grammar: " + plainGrammar)
      println("Strings for student grammar: " + (new LazyGenerator(cnfG)).getIterator(30).map(_.mkString(" ")).mkString("\n"))
      //System.exit(0)
    }
    if (cnfG.rules.isEmpty) {
      clientLog("The grammar is empty. Not all rules are produtive and reachable !!")
    } else {

      //println("Student Grammar In GNF: " + GNFConverter.toGNF(cnfG))
      //proveEquivalence(equivChecker.cnfRef, cnfG)

      equivChecker.isEquivalentTo(cnfG) match {
        case equivResult @ PossiblyEquivalent() => {
          clientLog(s"System:  $equivResult")
          proveEquivalence(equivChecker.cnfRef, cnfG)
        }
        case equivResult @ NotEquivalentNotAcceptedBySolution(_) =>
          clientLog(s"System:  Wrong. $equivResult")
          val resG = repairer.repair(cnfG, equivResult)
          clientLog("Repaired Grammar: ")
          clientLog(resG.toString)
          proveEquivalence(equivChecker.cnfRef, resG)
          
        case equivResult @ NotEquivalentNotGeneratedBySolution(_) =>
          clientLog(s"System:  Wrong. $equivResult")
          val resG = repairer.repair(cnfG, equivResult)
          clientLog("Repaired Grammar: ")
          clientLog(resG.toString)
          proveEquivalence(equivChecker.cnfRef, resG)
      }
      clientLog(GrammarUtils.isLL1WithFeedback(plainGrammar).map("LL1:     " + _).getOrElse("LL1:     OK"))
    }
    clientLog("===============")
    /*//for stats    
    val pr = new java.io.PrintWriter(quiz.quizName + "-stats.txt")
    Stats.dumpStats(pr)
    pr.close()*/

    def proveEquivalence(g1: Grammar, g2: Grammar) = {
      clientLog("Trying to prove equivalence...")
      val verifier = new EquivalenceVerifier(g1, g2, nOfTests)
      verifier.proveEquivalence() match {
        case Some(true) =>
          clientLog("The grammars are proved to be equivalent!")
        case _ =>
          clientLog("Cannot prove equivalence!")
      }
    }
  }
  
  
}

