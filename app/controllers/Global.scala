import java.io.File
import play.api._
import controllers.MyLogger
import controllers.Manage
import play.api.mvc.{EssentialAction, Handler, SimpleResult, RequestHeader}
import scala.concurrent.Future
import models.FamillyNames

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    println("システムスタート")
  }

  override def onStop(app: Application) {
    Manage.backup()
    println("システム終了")
  }
}