package importer

import java.io.File
import scala.io.Source

class GRRMImporter(comfile: File, needsMultiframe: Boolean) extends ReactionMapImporter(comfile: File, needsMultiframe: Boolean) {

  def findPrefix():String = comfile.getPath.substring(0, comfile.getPath.length - 4)
  def readGRRM():String = Source.fromFile(prefix + ".com").mkString

  def readEQ(): List[EQ] = {
    var eqs: List[EQ] = List()
    val filename = prefix + "_EQ_list.log"
    val listFile = Source.fromFile(filename)
    var geometry: Option[List[List[Any]]] = None
    for (line <- listFile.getLines()) {
      if (line.startsWith("# Geometry of")) {
        geometry = Some(List())
      } else if (line.startsWith("Energy")) {
        val energy = line.split("=")(1).trim.toDouble
        eqs = EQ("EQ" + eqs.length, energy, geometry.get.reverse) :: eqs
      } else {
        geometry match {
          case Some(geom) =>
            val items = line.trim.split("\\s+")
            if (items.length == 4) {
              geometry = Some(List(items(0).trim, items(1).toDouble, items(2).toDouble, items(3).toDouble) :: geom)
            }
          case _ =>
        }
      }
    }
    eqs.reverse
  }

  def readTS(): List[TS] = {
    var tss: List[TS] = List()
    val filename = prefix + "_TS_list.log"
    val listFile = Source.fromFile(filename)
    var geometry: Option[List[List[Any]]] = None
    var g: List[List[Any]] = List()
    var energy: Double = 0.0
    var numSkips = 0
    for (line <- listFile.getLines()) {
      if (line.startsWith("# Geometry of")) {
        geometry = Some(List())
        energy = 0.0
      } else if (line.startsWith("Energy")) {
        energy = line.split("=")(1).trim.toDouble
        g = geometry.get.reverse
      } else if (line.startsWith("CONNECTION")) {
        val start = line.indexOf(':') + 1
        val end = line.indexOf('-')
        try {
          val eq1 = line.substring(start, end).trim.toInt
          val eq2 = line.substring(end + 1).trim.toInt
          val ts = TS("TS" + (tss.length + numSkips), energy, g, EQs(eq1), EQs(eq2))
          EQs(eq1).TSs = EQs(eq1).TSs ++ List(ts)
          EQs(eq2).TSs = EQs(eq2).TSs ++ List(ts)
          tss = ts :: tss
        } catch {
          case _: NumberFormatException => numSkips = numSkips + 1
        }
      } else {
        geometry match {
          case Some(geom) =>
            val items = line.trim.split("\\s+")
            if (items.length == 4) {
              geometry = Some(List(items(0).trim, items(1).toDouble, items(2).toDouble, items(3).toDouble) :: geom)
            }
          case _ =>
        }
      }
    }
    tss.reverse
  }

  def readDC(): List[DC] = {
    var dcs: List[DC] = List()
    val filename = prefix + "_DC_list.log"
    val listFile = Source.fromFile(filename)
    var geometry: Option[List[List[Any]]] = None
    for (line <- listFile.getLines()) {
      if (line.startsWith("# Geometry of")) {
        geometry = Some(List())
      } else if (line.startsWith("Energy")) {
        val energy = line.split("=")(1).trim.toDouble
        dcs = DC("DC" + dcs.length, energy, geometry.get.reverse) :: dcs
      } else if (line.startsWith("CONNECTION")) {
        val start = line.indexOf(':') + 1
        val end = line.indexOf('-')
        val eq = line.substring(start, end).trim.toInt
        dcs.head.eq = EQs(eq)
        EQs(eq).DCs = EQs(eq).DCs ++ List(dcs.head)
      } else {
        geometry match {
          case Some(geom) =>
            val items = line.trim.split("\\s+")
            if (items.length == 4) {
              geometry = Some(List(items(0).trim, items(1).toDouble, items(2).toDouble, items(3).toDouble) :: geom)
            }
          case _ =>
        }
      }
    }
    dcs.reverse
  }
}
