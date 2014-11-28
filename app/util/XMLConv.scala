package util

import scala.io.Source
import java.io.PrintWriter
import java.io.File
import scala.xml.XML

// データをXML形式に相互変換する
trait XMLConv extends Utilities {

  // データをXML形式でデシリアライズ、中身がないなら空のリストを返す
  protected def readXML[E](path: String)(fromXML: scala.xml.Node => E): List[E] = if (new File(path) exists) {
    val items = XML.loadString(using(Source.fromFile(path, "UTF-8")) { _.getLines.mkString }.getOrElse("")) \\ "item"
    items.map(fromXML(_)).toList
  } else {
    List.empty
  }

  // データをXML形式でシリアライズ
  protected def writeXML[E <: { def toXML: scala.xml.NodeBuffer }](path: String, list: List[E]) =
    using(new PrintWriter(path, "UTF-8")) {
      // 改行コードをXMLが認識できる形に変換した結果こんなものが生まれてしまった
      _.write {
        s"""<?xml version="1.0" encoding="UTF-8" ?>
         <root>
           <items>
              ${list.map(_.toXML.map(_.toString.lines.mkString("&#xA;")).mkString("")).map(x => s"<item>${x}</item>").mkString("")}
           </items>
         </root>"""
      }
    }

}