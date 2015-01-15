package controllers

import util.Utilities
import scala.concurrent._
import ExecutionContext.Implicits.global
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.File

object MyLogger extends Utilities {

  val Path = s"./logs/${System.currentTimeMillis()}log.txt"
  val dir = new File("./logs")
  if (!dir.exists()) dir.mkdir()

  // 文字列を受け取ったらロギングを別のスレッドにまかせてすぐ帰る
  def log(str: String) {
    // 実際にログを取る関数、排他制御
    def prvLog(str: String): Any = synchronized {
      //scalax.file.Path(Path).append(s"${str}\r\n")
      val writer = new OutputStreamWriter(new FileOutputStream(Path, true), "utf-8")
      writer.write(s"${str}\r\n")
      writer.close
    }
    println(str)
    future { prvLog(str) }
  }

}