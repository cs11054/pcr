package models

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import util.Utilities
import util.XMLConv

case class User(id: String, password: String) {
  def toXML = <id>{ id }</id>
              <password>{ password }</password>
}

object Users extends Table[User]("USER") with DBSupport with XMLConv {

  def id = column[String]("ID", O.PrimaryKey)
  def password = column[String]("PASSWORD", O.NotNull)
  def ins = id ~ password

  def * = id ~ password <> (User, User.unapply _)
  val ANONY = "??"
  val SAVE_NAME = "users.xml"

  def save(path: String) { writeXML(path + SAVE_NAME, all()) }

  def load(path: String) {
    val list = readXML(path + SAVE_NAME) { node =>
      val id = (node \ "id").text
      val password = (node \ "password").text
      User(id, password)
    }
    list.foreach(add(_))
  }

  def add(u: User) = connectDB {
    if (!Query(Users).list().exists(x => x.id == u.id))
      Users.ins.insert(u.id, u.password)
  }

  def isRegistered(id: String, password: String): Boolean = connectDB {
    Query(Users).list().exists(u => u.id == id && u.password == password)
  }

  def isRegisteredID(id: String, password: String): Boolean = connectDB {
    Query(Users).list().exists(u => u.id == id)
  }

  def add(id: String, password: String) = connectDB {
    if (!isRegisteredID(id, password)) Users.ins.insert(id, password)
    else 0
  }

  def getName(id: String): String = if (!id.startsWith(ANONY)) id else FamillyNames.anony2famname(id)

  def allDel() = connectDB {
    Query(Users).filter(_.id =!= "cs11054").delete
  }

  def delete(id: String) = connectDB {
    if (id != "cs11054") Users.filter(_.id === id).delete
    else 0
  }

  def all(): List[User] = connectDB {
    Query(Users).sortBy(_.id).list
  }

}