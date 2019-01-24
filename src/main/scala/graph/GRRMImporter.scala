package graph

import java.io.{InputStream, OutputStream, File}

import geometry.Point

import scala.io.Source
import scala.util.Random
import sys.process._

/**
  * Created by tomohiro on 2016/02/29.
  */

class GRRMImporter(dirname: File, comfilename: String) {
  def arrange(nodes:List[Node]) = {
      var i = 0
      while (nodes.exists(_.arrange()) && i < nodes.size) {
        i = i + 1
      }

  }
  val prefix: String = dirname + "/" + comfilename.subSequence(0, comfilename.length - 4)
  val grrm: String = Source.fromFile(prefix + ".com").mkString
  val EQs0: List[EQ] = readEQ()
  val num_grids: Int = Math.ceil(Math.sqrt(EQs0.length) + 0.5).toInt
  val EQs: List[EQ] = EQs0 ++ (0 until num_grids * num_grids - EQs0.length).map((i: Int) => EQ("DEQ" + i, 0.0, List()))
  val TSs: List[TS] = readTS()
  val DCs: List[DC] = readDC()
  val gap: Point = Point(1.0 / num_grids.toDouble, 1.0 / num_grids.toDouble)
  val shuffledEQs: List[EQ] = EQs.map((_, Random.nextDouble())).sortBy(_._2).map(_._1)
  for (index <- shuffledEQs.indices) {
    val eq: EQ = EQs(index)
    val x: Double = index % num_grids
    val y: Double = index / num_grids + x % 2 * 0.5
    eq.position = Point(gap.x * (x + 0.5), gap.y * (y + 0.5))
  }
  arrange(EQs)
  arrange(TSs)
  arrange(DCs)
  val vertices = EQs0 ++ DCs ++ TSs

  readBonds()
  readSmiles()
  readInchi()
  readCanost()
  val edges: List[Map[String, BigInt]] =
    DCs.map((dc: DC) => Map("vertex1" -> BigInt(vertices.indexOf(dc)), "vertex2" -> BigInt(vertices.indexOf(dc.eq)))) ++
      TSs.map((ts: TS) => Map("vertex1" -> BigInt(vertices.indexOf(ts)), "vertex2" -> BigInt(vertices.indexOf(ts.eq1)))) ++
      TSs.map((ts: TS) => Map("vertex1" -> BigInt(vertices.indexOf(ts)), "vertex2" -> BigInt(vertices.indexOf(ts.eq2))))
  val toJSON: Map[String, Any] = Map("vertices" -> vertices.map(_.toJSON), "edges" -> edges, "grrm" -> grrm)

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


  abstract class Node(label: String, energy: Double, geometry: List[List[Any]]) {
    var position: Point = Point(-1.0, -1.0)

    def origin: Point

    def extent: Point

    var bonds: List[List[BigInt]] = List()
    var smiles = ""
    var inchi = ""
    var sdf = ""
    var canost: Option[String] = None

    def dist(node: Node) = position.dist(node.position)

    def _random_position = Point(origin.x + extent.x * Random.nextDouble(), origin.y + extent.y * Random.nextDouble())

    def arrange(): Boolean = {
      position = (0 until 100)
        .map((_: Int) => Point(origin.x + extent.x * Random.nextDouble(), origin.y + extent.y * Random.nextDouble()))
        .map((p: Point) => (p, (EQs ++ TSs ++ DCs)
          .filter(_ != this)
          .map((n: Node) => {
            val d = n.position.dist(p)
            if (d < 0.1)
              d + Math.abs(n.position.y - p.y)
            else
              d
          })
          .fold(Double.MaxValue)(Math.min)))
        .maxBy(_._2)._1
      false
    }

    def toJSON: Map[String, Any] = {
      val json = Map(
        "position" -> List(position.x, position.y),
        "label" -> label,
        "energy" -> energy,
        "geometry" -> geometry,
        "bonds" -> bonds,
        "smiles" -> smiles,
        "inchi" -> inchi,
        "sdf" -> sdf)
      canost match {
        case Some(c) => json + ("canost" -> c)
        case None => json
      }
    }
  }

  case class EQ(label: String, energy: Double, geometry: List[List[Any]])
    extends Node(label: String, energy: Double, geometry: List[List[Any]]) {
    var TSs: List[TS] = List()
    var DCs: List[DC] = List()

    def origin = position - gap * 0.5

    def extent = gap

    val isDummy = false

    override def arrange(): Boolean = {
      def total_ts_dist(p: Point, eq: EQ) = eq.TSs.map(_.peer(eq).position.dist(p)).sum
      val self_d: Double = total_ts_dist(position, this)
      val best = EQs
        .map(
          (eq: EQ) => (
            self_d + total_ts_dist(eq.position, eq)
              - (total_ts_dist(eq.position, this) + total_ts_dist(this.position, eq)),
            eq))
        .maxBy(_._1)
      if (best._1 > 1.0e-8) {
        val preScore = TSs.map(_.dist_between_eqs).sum
        val best_eq = best._2
        val p1 = position
        val p2 = best_eq.position
        position = p2
        best_eq.position = p1
        val postScore = TSs.map(_.dist_between_eqs).sum
        if (postScore  < preScore) {
          true
        } else {
          position = p1
          best_eq.position = p2
          false
        }
      } else {
        false
      }
    }
  }

  case class TS(label: String, energy: Double, geometry: List[List[Any]], eq1: EQ, eq2: EQ)
    extends Node(label: String, energy: Double, geometry: List[List[Any]]) {

    def origin =
      if (eq1 == eq2)
        eq1.origin
      else
        Point(Math.min(eq1.position.x, eq2.position.x), Math.min(eq1.position.y, eq2.position.y))

    def extent =
      if (eq1 == eq2)
        eq1.extent
      else
        Point(Math.abs(eq1.position.x - eq2.position.x), Math.abs(eq1.position.y - eq2.position.y))

    def dist_between_eqs = eq1.position.dist(eq2.position)

    def peer(eq: EQ) = if (eq == eq1) eq2 else if (eq == eq2) eq1 else null

    val _multiframe: (List[(String, Double)], BigInt) = {
      val chunks = Source.fromFile(prefix + "_" + label + ".log").mkString.split("=========================================================================")
      val header = "" + geometry.length + "\n\n"
      val ts = List((header + chunks(0).split("\nENERGY")(0).split("\n").drop(2).mkString("\n"), energy))
      val forward_frames = chunks(1).split("\n\n").filter(_.startsWith("# STEP ")).map((step: String) => (
        header + step.split("\nENERGY")(0).split("\n").drop(1).mkString("\n"),
        step.split("\n").reverse(1).split("=")(1).trim.toDouble)).toList
      val backward_frames = chunks(3).split("\n\n").filter(_.startsWith("# STEP ")).map((step: String) => (
        header + step.split("\nENERGY")(0).split("\n").drop(1).mkString("\n"),
        step.split("\n").reverse(1).split("=")(1).trim.toDouble)).toList
      if (eq1.label <= eq2.label)
        (forward_frames.reverse ++ ts ++ backward_frames, forward_frames.length)
      else
        (backward_frames.reverse ++ ts ++ forward_frames, backward_frames.length)

    }

    override def toJSON: Map[String, Any] = super.toJSON +
      ("multiframe_xyz" -> _multiframe._1.map(_._1)) +
      ("multiframe_energies" -> _multiframe._1.map(_._2)) +
      ("multiframe_index" -> _multiframe._2)
  }

  case class DC(label: String, energy: Double, geometry: List[List[Any]])
    extends Node(label: String, energy: Double, geometry: List[List[Any]]) {
    var eq: EQ = null

    def origin = eq.origin

    def extent = eq.extent

    val _multiframe: List[(String, Double)] = {
      val chunks = Source.fromFile(prefix + "_" + label + ".log").mkString.split("=========================================================================")
      val header = "" + geometry.length + "\n\n"
      val dc = List((header + chunks(0).split("\nENERGY")(0).split("\n").drop(2).mkString("\n"), energy))
      val frames = chunks(1).split("\n\n").filter(_.startsWith("# STEP "))
      val forward_frames = frames.map((step: String) => (
        header + step.split("\nENERGY")(0).split("\n").drop(1).mkString("\n"),
        step.split("\n").reverse(1).split("=")(1).trim.toDouble)).toList
      forward_frames.reverse ++ dc
    }

    override def toJSON: Map[String, Any] = super.toJSON +
      ("multiframe_xyz" -> _multiframe.map(_._1)) +
      ("multiframe_energies" -> _multiframe.map(_._2))
  }

  def readBonds() = {
    val babel = Process("babel -i xyz - -o mol") // sd
    val babel_io = new ProcessIO(
        allXYZs: ((OutputStream) => Unit),
        collectBonds: ((InputStream) => Unit),
        voidErr: ((InputStream) => Unit))
    (babel run babel_io).exitValue
  }

  def readSmiles() = {
    val babel = Process("babel -i xyz - -o can")
    val babel_io = new ProcessIO(
      allXYZs: ((OutputStream) => Unit),
      collectSmiles: ((InputStream) => Unit),
      voidErr: ((InputStream) => Unit))
    (babel run babel_io).exitValue
  }

  def readInchi() = {
    val babel = Process("babel -i xyz - -o inchi")
    val babel_io = new ProcessIO(
      allXYZs: ((OutputStream) => Unit),
      collectInchi: ((InputStream) => Unit),
      voidErr: ((InputStream) => Unit))
    (babel run babel_io).exitValue
  }

  def readCanost() = {
    for (node: Node <- (EQs0 ++ DCs ++ TSs)) {
      val canost = Process("./main_canost -u f /dev/stdin /dev/stdout")
      val canost_io = new ProcessIO(
        (stream: OutputStream) => {
          stream.write(node.sdf.getBytes())
          stream.close()
        },
        (stream: InputStream) => {
          val lines = Source.fromInputStream(stream).getLines
          node.canost = Some(lines.next())
          if (node.canost.get.trim.isEmpty) {
            lines.next()
            node.canost = Some(lines.next().trim)
          } else {
            node.canost = None
          }
        },
        voidErr: ((InputStream) => Unit))
      (canost run canost_io).exitValue
    }
  }

  def allXYZs(out: OutputStream): Unit = {
    def write(geometry: List[List[Any]]) = {
      out.write(("" + (geometry.length) + "\n\n" +
        geometry.map((atom: List[Any]) =>
          atom(0).asInstanceOf[String] + " " +
            atom(1).asInstanceOf[Double] + " " +
            atom(2).asInstanceOf[Double] + " " +
            atom(3).asInstanceOf[Double]).mkString("\n") + "\n").getBytes)
    }
    for (eq: EQ <- EQs0)
      write(eq.geometry)
    for (dc: DC <- DCs)
      write(dc.geometry)
    for (ts: TS <- TSs)
      write(ts.geometry)
    out.close()
  }

  def collectBonds(in: InputStream): Unit = {
    val stdout = Source.fromInputStream(in).getLines
    def read(node: Node) = {
      var lines: List[String] = List()
      def readline(): String = {
        val line: String = stdout.next()
        lines = line :: lines
        line
      }
      readline()
      readline()
      readline()
      val header = readline().trim.split("\\s+")
      for (_ <- 0 until header(0).toInt)
        readline()
      for (_ <- 0 until header(1).toInt) {
        val columns = readline().trim.split("\\s+")
        node.bonds = node.bonds ++ List(
          List(
            BigInt(columns(0).toInt - 1),
            BigInt(columns(1).toInt - 1),
            BigInt(columns(2).toInt)))
      }
      while (stdout.hasNext && readline.trim != "$$$$") {}
      node.sdf = lines.reverse.mkString("\n")
    }
    for (eq: EQ <- EQs0)
      read(eq)
    for (dc: DC <- DCs)
      read(dc)
    for (ts: TS <- TSs)
      read(ts)
  }

  def collectSmiles(in: InputStream): Unit = {
    val stdout = Source.fromInputStream(in).getLines
    def read(node: Node) = {
      node.smiles = stdout.next().trim.split("\t")(0).trim
    }
    for (eq: EQ <- EQs0)
      read(eq)
    for (dc: DC <- DCs)
      read(dc)
    for (ts: TS <- TSs)
      read(ts)
  }

  def collectInchi(in: InputStream): Unit = {
    val stdout = Source.fromInputStream(in).getLines
    def read(node: Node) = {
      var line = stdout.next()
      while (!line.startsWith("InChI="))
        line = stdout.next()
      node.inchi = line.substring(6).trim
    }
    for (eq: EQ <- EQs0)
      read(eq)
    for (dc: DC <- DCs)
      read(dc)
    for (ts: TS <- TSs)
      read(ts)
  }

  def voidErr(in: InputStream): Unit = ()

}
