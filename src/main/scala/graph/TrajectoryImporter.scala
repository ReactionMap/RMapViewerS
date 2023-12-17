package graph

import java.io.{File, FileNotFoundException}
import scala.io.Source
import scala.swing.Dialog

class TrajectoryImporter(dirname: File, comfilename: String)
  extends ReactionMapImporter(dirname: File, false) {

  def findPrefix():String = dirname + "/" + comfilename.subSequence(0, comfilename.length - 12)
  def readGRRM():String = {
    ""
//    try {
//      Source.fromFile(prefix + ".com").mkString
//    } catch {
//      case e: FileNotFoundException => "Not Available"
//    }
  }

  def readEQ(): List[EQ] = {
    var eqs: List[EQ] = List()
    val filename = prefix + "_EQ_list.xyz"
    val lines: Iterator[String] = Source.fromFile(filename).getLines()
    println("prifix: "+prefix)
    while (lines.hasNext) {
      val numLines: Int = lines.next().trim.toInt
      val comments: Array[String] = lines.next().split(',')
      val energy: Double = comments(2).split('=')(1).trim.toDouble
      var geometry: List[List[Any]] = List()
      for {l <- 1 to numLines} {
        val items: Array[String] = lines.next().trim.split("\\s+")
        geometry = List(items(0).trim, items(1).toDouble, items(2).toDouble, items(3).toDouble) :: geometry
      }
      eqs = EQ("EQ" + eqs.length, energy, geometry.reverse) :: eqs
    }
    eqs.reverse
  }
  def readTS(): List[TS] = {
    var tss: List[TS] = List()
    val filename = prefix + "_TS_list.xyz"
    val lines: Iterator[String] = Source.fromFile(filename).getLines()
    while (lines.hasNext) {
      val numLines: Int = lines.next().trim.toInt
      val comments: Array[String] = lines.next().split(',')
      val energy: Double = comments(2).split('=')(1).trim.toDouble
      val connection = comments(3).split(':')(1).split('-')
      val eq1: String = connection(0).trim
      val eq2: String = connection(1).trim
      if (eq1 != "DC" && eq2 != "DC") {
        var geometry: List[List[Any]] = List()
        for {l <- 1 to numLines} {
          val items: Array[String] = lines.next().trim.split("\\s+")
          geometry = List(items(0).trim, items(1).toDouble, items(2).toDouble, items(3).toDouble) :: geometry
        }
        tss = TS("TS" + tss.length, energy, geometry.reverse, EQs(eq1.toInt), EQs(eq2.toInt)) :: tss
      } else {
        for {l <- 1 to numLines}
          lines.next()
      }
    }
    tss.reverse
  }
  def readDC(): List[DC] = List()
}
