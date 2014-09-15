package controllers

import play.api._
import play.api.mvc._

import akka.actor._

import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._
import play.api.libs.concurrent.Akka

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

import akka.util.Timeout
import akka.pattern.ask

import models.Protocol._

object Application extends Controller {

  def index = Action { implicit r =>
    Ok(views.html.index())
  }
  
  def openConsole() = WebSocket.tryAccept[JsValue] { request =>
    import play.api.Play.current

    val session = Akka.system.actorOf(Props(new models.WebSession(request.remoteAddress)))
    implicit val timeout = Timeout(1.seconds)

    (session ? Init).map {
      case InitSuccess(enumerator) =>
        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          session ! FromClient(event)
        }.map { _ =>
          session ! Quit
        }

        Right((iteratee, enumerator))

      case InitFailure(error: String) =>
        // Connection error
        Left(InternalServerError("Failed to create websocket"))
    }
  }

}
