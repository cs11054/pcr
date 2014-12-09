package models

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import java.util.Date
import util.Utilities
import util.XMLConv

case class Comment(subjectid: Int, userid: String, commentid: Int, postUser: String, body: String,
  date: Long, isNew: Boolean, viewid: String, res: Boolean) {

  def formatDate(form: String = "yyyy/mm/dd hh:mm"): String = form match {
    case "yyyy/mm/dd hh:mm" => "%tY/%<tm/%<td %<tR" format new Date(date)
    case _ => date.toString()
  }

  def toXML = <subjectid>{ subjectid }</subjectid>
              <userid>{ userid }</userid>
              <commentid>{ commentid }</commentid>
              <postUser>{ postUser }</postUser>
              <body>{ body }</body>
              <date>{ date }</date>
              <isNew>{ isNew }</isNew>
              <viewid>{ viewid }</viewid>
              <res>{ res }</res>
}

object Comments extends Table[Comment]("COMMENT") with DBSupport with XMLConv {

  def subjectid = column[Int]("SUBJECTID", O.PrimaryKey, O.NotNull)
  def userid = column[String]("USERID", O.PrimaryKey, O.NotNull)
  def commentid = column[Int]("COMMENTID", O.PrimaryKey, O.NotNull)
  def postUser = column[String]("POSTUSER", O.NotNull)
  def body = column[String]("BODY")
  def date = column[Long]("DATE", O.NotNull)
  def isNew = column[Boolean]("NEW", O.NotNull)
  def viewid = column[String]("VIEWID", O.NotNull)
  def res = column[Boolean]("RES", O.NotNull)
  def * = subjectid ~ userid ~ commentid ~ postUser ~ body ~ date ~ isNew ~ viewid ~ res <> (Comment, Comment.unapply _)
  def ins = subjectid ~ userid ~ commentid ~ postUser ~ body ~ date ~ isNew ~ viewid ~ res
  val SAVE_NAME = "comments.xml"

  def save(path: String) { writeXML(path + SAVE_NAME, all()) }

  def load(path: String) {
    val list = readXML(path + SAVE_NAME) { node =>
      val sid = (node \ "subjectid").text.toInt
      val uid = (node \ "userid").text
      val cid = (node \ "commentid").text.toInt
      val postUser = (node \ "postUser").text
      val body = (node \ "body").text
      val date = (node \ "date").text.toLong
      val isNew = (node \ "isNew").text.toBoolean
      val viewid = (node \ "viewid").text
      val res = (node \ "res").text.toBoolean
      Comment(sid, uid, cid, postUser, body, date, isNew, viewid, res)
    }
    list.foreach(add(_))
  }

  def add(c: Comment) = connectDB {
    if (!Query(Comments).list().exists(x => x.subjectid == c.subjectid && x.userid == c.userid && x.commentid == c.commentid))
      Comments.ins.insert(c.subjectid, c.userid, c.commentid, c.postUser, c.body, c.date, c.isNew, c.viewid, c.res)
  }

  def exists(sid: Int, uid: String): Boolean = connectDB {
    !Query(Comments).filter(x => x.subjectid === sid && x.viewid === uid).list().isEmpty
  }

  def all(): List[Comment] = connectDB {
    Query(Comments).sortBy(_.date).list
  }

  def allDel() = connectDB {
    Query(Comments).delete
  }

  def commentsOfTask(sid: Int, uid: String): List[Comment] = connectDB {
    Query(Comments).filter(c => c.subjectid === sid && c.userid === uid)
      .sortBy(_.date).list
  }

  def countComment(sid: Int, uid: String): Int = commentsOfTask(sid, uid).size

  def add(subjectid: Int, userid: String, postUser: String, body: String, viewid: String, resTrgIds: List[Int]) = connectDB {
    val nextid = Query(Comments).filter(t => t.subjectid === subjectid && t.userid === userid).list.size + 1
    val date = System.currentTimeMillis()
    val taskuser = Tasks.taskuserFromAnony(userid).getOrElse(userid)
    Comments.ins.insert(subjectid, userid, nextid, postUser, body, date, taskuser != postUser, viewid, false)
    for (trg <- resTrgIds) {
      Query(Comments).filter(t => t.subjectid === subjectid && t.userid === userid && t.commentid === trg).map(_.res).update(true)
    }
    nextid
  }

  def delete(subjectid: Int, userid: String, commentID: Int) = connectDB {
    Comments.filter(c => c.subjectid === subjectid && c.userid === userid &&
      c.commentid === commentid).delete
  }

  def recvNewsAndRecentUntilN(id: String, n: Int): List[Comment] = connectDB {
    val ids = Tasks.userTaskIds(id)
    val cmts = recvCmts(ids)
    val news = cmts.filter(c => c.isNew && c.postUser != id)
    val limit = if (n >= news.size) n - news.size else 0
    val ret = news ::: cmts.sortBy(_.date)(Desc).take(limit)
    ids.foreach(id => Query(Comments).filter(c => c.userid === id && c.isNew).map(_.isNew).update(false))
    ret
  }

  def recvCmts(ids: List[String]): List[Comment] = connectDB {
    ids.flatMap(id => Query(Comments).filter(c => c.userid === id).list).sortBy(_.date)(Desc)
  }

  def newComments(id: String): List[Comment] = connectDB {
    val ret = Query(Comments).filter(c => c.userid === id && c.isNew && c.postUser =!= id).sortBy(_.date.desc).list
    Query(Comments).filter(c => c.userid === id && c.isNew).map(_.isNew).update(false)
    ret
  }

  def recvNComments(id: String, limit: Int): List[Comment] = connectDB {
    Query(Comments).filter(c => c.userid === id && c.postUser =!= id).sortBy(_.date.desc).take(limit).list
  }

  def postNComments(id: String, limit: Int): List[Comment] = connectDB {
    Query(Comments).filter(c => c.postUser === id).sortBy(_.date.desc).take(limit).list
  }

  def postNCmtsAndRes(id: String, n: Int): List[Comment] = connectDB {
    val ress = Query(Comments).filter(c => c.res && c.postUser === id).sortBy(_.date.desc).list
    val limit = if (n >= ress.size) n - ress.size else 0
    val ret = ress ::: Query(Comments).filter(c => c.postUser === id && !c.res).sortBy(_.date.desc).take(limit).list
    Query(Comments).filter(c => c.postUser === id && c.res).map(_.res).update(false)
    ret
  }
}