package util

import java.util.Date

trait Utilities {

  protected def using[A <: { def close() }, B](resource: A)(func: A => B): Option[B] =
    try {
      Some(func(resource)) // 成功したら、Someに包んで返す
    } catch {
      case e: Exception =>
        e.printStackTrace
        None // 失敗したら、ログ吐いて、None返す
    } finally {
      if (resource != null) resource.close()
    }

  def nowTime(): String = "%ty/%<tm/%<td %<tH:%<tM:%<tS" format new Date

}