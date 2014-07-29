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

    case FromClient(event) =>
      try {
        Logger.info("[<] "+event)

        (event \ "action").as[String] match {
          case "hello" =>
            clientLog("Welcome!")
          case _ =>
            clientError("Error: Unhandled client event "+event)
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

