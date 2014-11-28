package models

//import scala.slick.driver.H2Driver.simple._
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession

trait DBSupport {

  val Heroku = false

  val DB_PATH = if (!Heroku) "jdbc:h2:tcp://localhost:9093/db"
  else "jdbc:postgresql://localhost:5432/postgres" //"postgres://zcgmsoybywazlx:HQt5HBNiYxgDE4LdQ337Ry_urb@ec2-54-83-204-78.compute-1.amazonaws.com:5432/dcn5isfl4oitng"

  val DRIVER = if (DB_PATH.contains("localhost")) "org.h2.Driver" else "org.postgresql.Driver"
  val USER = if (DB_PATH.contains("localhost")) "sa" else "sa" //"zcgmsoybywazlx"
  val PASS = if (DB_PATH.contains("localhost")) null else "a" //"HQt5HBNiYxgDE4LdQ337Ry_urb"

  // ソートの逆順用
  def Desc[T: Ordering] = implicitly[Ordering[T]].reverse

  // DBへの接続を補助
  def connectDB[T](f: => T): T = {
    Database.forURL(DB_PATH, driver = DRIVER, user = USER, password = PASS) withSession {
      f
    }
  }

}