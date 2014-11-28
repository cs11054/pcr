package models

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import scala.concurrent.SyncVar
import util.Utilities
import util.XMLConv

case class Subject(subjectid: Int, name: String) {
  def toXML = <subjectid>{ subjectid }</subjectid>
              <name>{ name }</name>
}

object Subjects extends Table[Subject]("SUBJECT") with DBSupport with XMLConv {

  def subjectid = column[Int]("SUBJECTID", O.AutoInc, O.PrimaryKey, O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def * = subjectid ~ name <> (Subject, Subject.unapply _)
  def ins = name returning subjectid
  def myins = subjectid ~ name

  private val snameMap = scala.collection.concurrent.TrieMap.empty[Int, String]
  val SAVE_NAME = "subjects.xml"

  def save(path: String) { writeXML(path + SAVE_NAME, all()) }

  def load(path: String) {
    val list = readXML(path + SAVE_NAME) { node =>
      val subjectid = (node \ "subjectid").text.toInt
      val name = (node \ "name").text
      Subject(subjectid, name)
    }
    list.foreach(add(_))
  }

  def add(s: Subject) = connectDB {
    if (!Query(Subjects).list().exists(x => x.subjectid == s.subjectid))
      Subjects.myins.insert(s.subjectid, s.name)
  }

  def allDel() = connectDB {
    Query(Subjects).delete
  }

  def add(name: String) = connectDB {
    Subjects.ins.insert(name)
  }

  def delete(id: Int) = connectDB {
    Subjects.filter(_.subjectid === id).delete
  }

  def all(): List[Subject] = connectDB {
    Query(Subjects).sortBy(_.subjectid.desc).list
  }

  def getTitle(sid: Int): String = snameMap.get(sid) match {
    case Some(x) => x
    case None =>
      getTitleFromDB(sid) match {
        case Some(x) =>
          snameMap.put(sid, x)
          x
        case None => sid.toString()
      }
  }

  private def getTitleFromDB(sid: Int): Option[String] = connectDB {
    Query(Subjects).filter(_.subjectid === sid).map(_.name).firstOption
  }

  // 一番新しい課題の番号を取得、ないなら-1を返す
  def newestNum(): Int = all().map(_.subjectid).headOption.getOrElse(-1)

}