package util

import java.security.MessageDigest

// 匿名投稿に対し重複しない値を設定
object Anonymouser {

  private val md = MessageDigest.getInstance("SHA-1")

  def anonyNum(salt: List[Any]): String = {
    md.digest(salt.mkString.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

}