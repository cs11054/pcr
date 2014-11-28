package models

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import java.util.Date
import util.XMLConv

case class Iine(subjectid: Int, userid: String, pushUser: String, date: Long, isNew: Boolean) {

  def formatDate(form: String = "yyyy/mm/dd hh:mm"): String = form match {
    case "yyyy/mm/dd hh:mm" => "%tY/%<tm/%<td %<tR" format new Date(date)
    case _ => date.toString()
  }

  def toXML = <subjectid>{ subjectid }</subjectid>
              <userid>{ userid }</userid>
              <pushUser>{ pushUser }</pushUser>
              <date>{ date }</date>
              <isNew>{ isNew }</isNew>

}

object Iines extends Table[Iine]("IINE") with DBSupport with XMLConv {

  def subjectid = column[Int]("SUBJECTID", O.PrimaryKey, O.NotNull)
  def userid = column[String]("USERID", O.PrimaryKey, O.NotNull)
  def pushUser = column[String]("PUSHUSER", O.PrimaryKey, O.NotNull)
  def date = column[Long]("DATE", O.NotNull)
  def isNew = column[Boolean]("NEW", O.NotNull)
  def * = subjectid ~ userid ~ pushUser ~ date ~ isNew <> (Iine, Iine.unapply _)
  def ins = subjectid ~ userid ~ pushUser ~ date ~ isNew
  val SAVE_PATH = "iines.xml"

  def save(path: String) { writeXML(path + SAVE_PATH, all()) }

  def load(path: String) {
    val list = readXML(path + SAVE_PATH) { node =>
      val sid = (node \ "subjectid").text.toInt
      val uid = (node \ "userid").text
      val pushUser = (node \ "pushUser").text
      val date = (node \ "date").text.toLong
      val isNew = (node \ "isNew").text.toBoolean
      Iine(sid, uid, pushUser, date, isNew)
    }
    list.foreach(add(_))
  }

  def add(i: Iine) = connectDB {
    if (!Query(Iines).list().exists(x => x.subjectid == i.subjectid && x.userid == i.userid && x.pushUser == i.pushUser))
      Iines.ins.insert(i.subjectid, i.userid, i.pushUser, i.date, i.isNew)
  }

  def all(): List[Iine] = connectDB {
    Query(Iines).sortBy(_.date).list
  }

  def allDel() = connectDB {
    Query(Iines).delete
  }

  def iineOfTask(sid: Int, uid: String): List[Iine] = connectDB {
    Query(Iines).filter(i => i.subjectid === sid && i.userid === uid).sortBy(_.date).list
  }

  def countIine(sid: Int, uid: String): Int = iineOfTask(sid, uid).size

  def add(sid: Int, uid: String, pushUser: String) = connectDB {
    val date = System.currentTimeMillis()
    val iines = iineOfTask(sid, uid)
    val IINEed = Query(Iines).filter(i => i.subjectid === sid && i.userid === uid && i.pushUser === pushUser).list.isEmpty
    if (IINEed) Iines.ins.insert(sid, uid, pushUser, date, true)
  }

  def recvNewsAndRecentUntilN(id: String, n: Int): List[Iine] = connectDB {
    val ids = Tasks.userTaskIds(id)
    val iines = recvIines(ids)
    val news = iines.filter(c => c.isNew && c.pushUser != id).sortBy(_.date)(Desc)
    val limit = if (n >= news.size) n - news.size else 0
    val ret = news ::: iines.filter(c => c.pushUser != id && !c.isNew).sortBy(_.date)(Desc).take(limit)
    ids.foreach(id => Query(Iines).filter(c => c.userid === id).map(_.isNew).update(false))
    ret
  }

  def recvIines(ids: List[String]): List[Iine] = connectDB {
    ids.flatMap(id => Query(Iines).filter(c => c.userid === id).list).sortBy(_.date)(Desc)
  }

  def delete() = ???

}