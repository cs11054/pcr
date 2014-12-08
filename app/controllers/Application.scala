package controllers

import models.{ Comments, Iines, Subjects, Tasks, Users }
import play.api.data.Forms._
import play.api.mvc._
import util.Utilities
import scala.io.Source
import play.api.data.Form
import util.Anonymouser
import util.Anonymouser

object Application extends Controller with myAuth with Utilities {

  val SEP = "<@>"

  // TOP	///////////////////////////////////////////////////
  def index = Authenticated { implicit request =>
    Redirect(routes.Application.subject(Subjects.newestNum))
  }

  // UserHome	///////////////////////////////////////////////
  def user = Authenticated { implicit request =>
    val id = request.session.get("user").get
    MyLogger.log(s"${id}${SEP}UserHome${SEP}${nowTime()}")
    Ok(views.html.userhome(id, Tasks.postNTasks(id, 5), Iines.recvNewsAndRecentUntilN(id, 5),
      Comments.postNCmtsAndRes(id, 5), Comments.recvNewsAndRecentUntilN(id, 5)))
  }

  // Subject	///////////////////////////////////////////////
  def subject(sid: Int, key: String = "date", msg: String = "") = Authenticated { implicit request =>
    MyLogger.log(s"${request.session.get("user").get}${SEP}Subject ${key} ${sid}${SEP}${nowTime()}")
    Ok(views.html.subject(sid, Subjects.all(), Tasks.sortedTasksOfSbj(sid, key), msg, sort = key))
  }

  // Task	///////////////////////////////////////////////////
  def task(sid: Int, uid: String, tid: Option[Int]) = Authenticated { implicit request =>
    MyLogger.log(s"${request.session.get("user").get}${SEP}Task ${sid}/${uid}${SEP}${nowTime()}")
    Ok(views.html.task(sid, uid, tid.getOrElse(Tasks.newestNumOfUser(sid, uid)),
      Tasks.getCaptionAndCode(sid, uid).zipWithIndex, Comments.commentsOfTask(sid, uid)))
  }

  // Comment	///////////////////////////////////////////////
  def cmtPost(sid: Int, tvid: String, tid: Int) = Authenticated { implicit req =>
    Form(tuple("body" -> text, "anonymous" -> optional(text), "resCmts" -> optional(text), "resHeader" -> optional(text))).bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.task(sid, tvid, tid,
        Tasks.getCaptionAndCode(sid, tvid).zipWithIndex,
        Comments.commentsOfTask(sid, tvid), msg = "コメントの投稿に失敗しました。")),
      cmt => {
        if (!Comments.exists(sid, tvid) || cmt._1.isEmpty()) {
          BadRequest(views.html.task(sid, tvid, tid, Tasks.getCaptionAndCode(sid, tvid).zipWithIndex,
            Comments.commentsOfTask(sid, tvid), msg = "コメントの投稿に失敗しました。"))
        } else {
          val user = req.session.get("user").get
          val vid = cmt._2 match {
            case Some(x) => Users.ANONY + Anonymouser.anonyNum(List(user, sid, tvid))
            case None => user
          }
          val ress = cmt._3 match {
            case Some(x) => x.split(",").toList
            case None => Nil
          }
          val header = cmt._4 match {
            case Some(x) => x + "\n"
            case None => ""
          }
          val trgRes = ress.flatMap { res =>
            try {
              Some(res.toInt)
            } catch {
              case e: Exception => None
            }
          }
          MyLogger.log(s"${req.session.get("user").get}${SEP}Comment ${sid}/${tvid}${SEP}${nowTime()}")
          Comments.add(sid, tvid, user, header + cmt._1, vid, trgRes)
          Ok(views.html.task(sid, tvid, tid, Tasks.getCaptionAndCode(sid, tvid).zipWithIndex, Comments.commentsOfTask(sid, tvid), msg = "コメントを投稿しました。"))
        }
      })
  }

  // IINE	////////////////////////////////////////////////////
  def iine(sid: Int, uid: String) = Authenticated { implicit req =>
    Ok(views.html.iine(sid, uid, Iines.countIineMap(sid, uid)))
  }

  def pushIine(sid: Int, uid: String, eff: Boolean = false, read: Boolean = false, strc: Boolean = false, help: Boolean = false) = Authenticated { implicit req =>
    val user = req.session.get("user").get
    val kind4Log = if (eff) "eff" else if (read) "read" else if (strc) "strc" else if (help) "help"
    MyLogger.log(s"${user}${SEP}Iine ${kind4Log} ${sid}/${uid}${SEP}${nowTime()}")
    Iines.add(sid, uid, user, eff, read, strc, help)
    Ok(views.html.iine(sid, uid, Iines.countIineMap(sid, uid)))
  }

  // Upload	///////////////////////////////////////////////////
  def upload = Action { implicit req =>
    Ok(views.html upload ())
  }

  def uploaded = Action(parse.multipartFormData) { req =>
    // いきなりPOSTしてくるハッカー対策、必要か不明
    if (req.session.get("user").isEmpty) Redirect(routes.Application.subject(Subjects.newestNum, msg = "投稿に失敗しました。"))
    // 形式がなぜか Map[String,Seq[String] なので、 Map[String,String] に変換
    val reqDate = req.body.asFormUrlEncoded.map(m => m._1 -> m._2.head)

    val sid = reqDate.getOrElse("sid", 0).toString().forall(_.isDigit) match {
      case true => reqDate.getOrElse("sid", 0).toString().toInt
      case false => 0
    }
    val user = req.session.get("user").get
    val vid = reqDate.get("anonymous") match {
      case Some(x) => Users.ANONY + Anonymouser.anonyNum(List(user, sid))
      case None => user
    }
    val caption = reqDate.get("caption").getOrElse("")

    req.body.file("source").map { src =>
      if (src.filename.endsWith(".java")) {
        val body = using(Source.fromFile(src.ref.file.getAbsoluteFile(), "UTF-8")) {
          _.getLines.mkString("\n")
        } getOrElse ("")
        src.ref.file.delete()
        if (body != "" && Tasks.exists(sid)) {
          Tasks.add(sid, user, caption, body, vid)
          MyLogger.log(s"${req.session.get("user").get}${SEP}Upload ${sid} ${caption.isEmpty()}${SEP}${nowTime()}")
          Redirect(routes.Application.subject(sid, msg = "投稿しました。"))
        } else {
          MyLogger.log(s"${req.session.get("user").get}${SEP}missUpload ${sid} ${caption.isEmpty()}${SEP}${nowTime()}")
          Redirect(routes.Application.subject(sid, msg = "投稿に失敗しました。"))
        }
      } else {
        Redirect(routes.Application.subject(sid, msg = "投稿に失敗しました。"))
      }
    }.getOrElse {
      Redirect(routes.Application.subject(sid, msg = "投稿に失敗しました。"))
    }
  }

  // Login	///////////////////////////////////////////////////
  def login = Action { implicit request =>
    Ok(views.html.login(loginForm))
  }

  def loginCheck = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors)),
      user => {
        MyLogger.log(s"${user._1}${SEP}Login${SEP}${nowTime()}")
        if (Users.isRegistered(user._1, user._2)) Redirect(routes.Application.index).withSession("user" -> user._1)
        else BadRequest(views.html.login(loginForm))
      })
  }

  def logout = Action { implicit request =>
    MyLogger.log(s"${session.get("user").getOrElse("???")}${SEP}Logout${SEP}${nowTime()}")
    Redirect(routes.Application.login).withNewSession
  }

}