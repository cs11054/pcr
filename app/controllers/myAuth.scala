package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._
import play.mvc.Security.Authenticated
import models.Users

trait myAuth extends Controller {

  case class AuthenticatedRequest(val user: String, request: Request[AnyContent])
    extends WrappedRequest(request)

  def Authenticated(f: AuthenticatedRequest => Result) = {
    Action { request =>
      request.session.get("user").map { user =>
        f(AuthenticatedRequest(user, request))
      }.getOrElse(Redirect(routes.Application.login()))
    }
  }

  def Administor(f: AuthenticatedRequest => Result) = {
    Action { request =>
      request.session.get("user").filter(_ == "cs11054").map { user =>
        f(AuthenticatedRequest(user, request))
      }.getOrElse(Redirect(routes.Application.login()))
    }
  }

  val loginForm = Form(tuple("id" -> text, "password" -> text)
    verifying("Invalid ID or password", result => result match {
    case (id, password) => check(id, password)
  }))

  def check(id: String, password: String) = {
    Users.isRegistered(id, password)
  }

}