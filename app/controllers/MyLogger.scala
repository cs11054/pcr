package controllers

import util.Utilities
import scala.concurrent._
import ExecutionContext.Implicits.global
import java.io.FileOutputStream

object MyLogger extends Utilities {

  val PATH = s"./logs/${System.currentTimeMillis()}log.txt"

  // 文字列を受け取ったらロギングを別のスレッドにまかせてすぐ帰る
  def log(str: String) {
    // 実際にログを取る関数、排他制御
    def prvLog(str: String): Any = synchronized {
      scalax.file.Path(PATH).append(s"${str}\r\n")
    }
    println(str)
    future { prvLog(str) }
  }

}