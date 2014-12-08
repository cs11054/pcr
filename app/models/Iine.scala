package models

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import java.util.Date
import util.XMLConv

case class Iine(subjectid: Int, userid: String, pushUser: String, efficiency: Boolean,
  readability: Boolean, structure: Boolean, helpful: Boolean, date: Long, isNew: Boolean) {

  def formatDate(form: String = "yyyy/mm/dd hh:mm"): String = form match {
    case "yyyy/mm/dd hh:mm" => "%tY/%<tm/%<td %<tR" format new Date(date)
    case _ => date.toString()
  }

  def toXML = <subjectid>{ subjectid }</subjectid>
              <userid>{ userid }</userid>
              <pushUser>{ pushUser }</pushUser>
              <efficiency>{ efficiency }</efficiency>
              <readability>{ readability }</readability>
              <structure>{ structure }</structure>
              <helpful>{ helpful }</helpful>
              <date>{ date }</date>
              <isNew>{ isNew }</isNew>

}

object Iines extends Table[Iine]("IINE") with DBSupport with XMLConv {

  def subjectid = column[Int]("SUBJECTID", O.PrimaryKey, O.NotNull)
  def userid = column[String]("USERID", O.PrimaryKey, O.NotNull)
  def pushUser = column[String]("PUSHUSER", O.PrimaryKey, O.NotNull)
  def efficiency = column[Boolean]("EFFICIENCY", O.NotNull)
  def readability = column[Boolean]("READABILITY", O.NotNull)
  def structure = column[Boolean]("STRUCTURE", O.NotNull)
  def helpful = column[Boolean]("HELPHUL", O.NotNull)
  def date = column[Long]("DATE", O.NotNull)
  def isNew = column[Boolean]("NEW", O.NotNull)
  def * = subjectid ~ userid ~ pushUser ~ efficiency ~ readability ~ structure ~ helpful ~ date ~ isNew <> (Iine, Iine.unapply _)
  def ins = subjectid ~ userid ~ pushUser ~ efficiency ~ readability ~ structure ~ helpful ~ date ~ isNew
  val SAVE_PATH = "iines.xml"

  def save(path: String) { writeXML(path + SAVE_PATH, all()) }

  def load(path: String) {
    val list = readXML(path + SAVE_PATH) { node =>
      val sid = (node \ "subjectid").text.toInt
      val uid = (node \ "userid").text
      val eff = (node \ "efficiency").text.toBoolean
      val read = (node \ "readability").text.toBoolean
      val strc = (node \ "structure").text.toBoolean
      val help = (node \ "helpful").text.toBoolean
      val pushUser = (node \ "pushUser").text
      val date = (node \ "date").text.toLong
      val isNew = (node \ "isNew").text.toBoolean
      Iine(sid, uid, pushUser, eff, read, strc, help, date, isNew)
    }
    list.foreach(add(_))
  }

  def add(i: Iine) = connectDB {
    if (!Query(Iines).list().exists(x => x.subjectid == i.subjectid && x.userid == i.userid && x.pushUser == i.pushUser))
      Iines.ins.insert(i.subjectid, i.userid, i.pushUser, i.efficiency, i.readability, i.structure, i.helpful, i.date, i.isNew)
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

  def countIine(sid: Int, uid: String): Int = iineOfTask(sid, uid).map { i =>
    var n = 0
    if (i.efficiency) n += 1; if (i.readability) n += 1
    if (i.structure) n += 1; if (i.helpful) n += 1
    n
  }.sum

  val EFF = "efficiency"; val READ = "readability"
  val STRC = "structure"; val HELP = "helphul"
  def countIineMap(sid: Int, uid: String): Map[String, Int] = {
    var map = scala.collection.mutable.Map.empty[String, Int]
    for (i <- iineOfTask(sid, uid)) {
      if (i.efficiency) map(EFF) = map.getOrElse(EFF, 0) + 1
      if (i.readability) map(READ) = map.getOrElse(READ, 0) + 1
      if (i.structure) map(STRC) = map.getOrElse(STRC, 0) + 1
      if (i.helpful) map(HELP) = map.getOrElse(HELP, 0) + 1
    }
    map.toMap
  }

  def add(sid: Int, uid: String, pushUser: String, eff: Boolean, read: Boolean, strc: Boolean, help: Boolean) = connectDB {
    val date = System.currentTimeMillis()
    val IINEed = !Query(Iines).filter(i => i.subjectid === sid && i.userid === uid && i.pushUser === pushUser).list.isEmpty
    if (IINEed && (eff || read || strc || help)) {
      if (eff) Query(Iines).filter(c => c.subjectid === sid && c.userid === uid && c.pushUser === pushUser).map(_.efficiency).update(true)
      if (read) Query(Iines).filter(c => c.subjectid === sid && c.userid === uid && c.pushUser === pushUser).map(_.readability).update(true)
      if (strc) Query(Iines).filter(c => c.subjectid === sid && c.userid === uid && c.pushUser === pushUser).map(_.structure).update(true)
      if (help) Query(Iines).filter(c => c.subjectid === sid && c.userid === uid && c.pushUser === pushUser).map(_.helpful).update(true)
      Query(Iines).filter(c => c.subjectid === sid && c.userid === uid && c.pushUser === pushUser).map(_.date).update(date)
      Query(Iines).filter(c => c.subjectid === sid && c.userid === uid && c.pushUser === pushUser).map(_.isNew).update(true)
    } else {
      Iines.ins.insert(sid, uid, pushUser, eff, read, strc, help, date, true)
    }
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
    ids.flatMap(id => Query(Iines).filter(i => i.userid === id).list).sortBy(_.date)(Desc)
  }

  def delete() = ???

}