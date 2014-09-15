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
}

