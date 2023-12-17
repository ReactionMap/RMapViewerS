package importer

import geometry.Point

import java.io.{File, InputStream, OutputStream}
import scala.io.Source
import scala.sys.process._
import scala.util.Random

/**
  * Created by tomohiro on 2016/02/29.
  */

abstract class ReactionMapImporter(file: File, needsMultiframe:Boolean)
{
  val prefix: String = findPrefix()
  val grrm: String = readGRRM()
  val EQs: List[EQ] = readEQ()
  val TSs: List[TS] = readTS()
  val DCs: List[DC] = readDC()
  val num_grids: Int = Math.ceil(Math.sqrt(EQs.length) + 0.5).toInt
  val gap: Point = Point(1.0 / num_grids.toDouble, 1.0 / num_grids.toDouble)
  val EQs1: List[EQ] = EQs ++ (0 until num_grids * num_grids - EQs.length).map((i: Int) => EQ("DEQ" + i, 0.0, List()))
  val vertices:List[Node] = EQs ++ DCs ++ TSs
  arrange()
  readBonds()
  readSmiles()
  readInchi()
  readCanost()
  val edges: List[Map[String, BigInt]] =
    DCs.map((dc: DC) => Map("vertex1" -> BigInt(vertices.indexOf(dc)), "vertex2" -> BigInt(vertices.indexOf(dc.eq)))) ++
      TSs.map((ts: TS) => Map("vertex1" -> BigInt(vertices.indexOf(ts)), "vertex2" -> BigInt(vertices.indexOf(ts.eq1)))) ++
      TSs.map((ts: TS) => Map("vertex1" -> BigInt(vertices.indexOf(ts)), "vertex2" -> BigInt(vertices.indexOf(ts.eq2))))
  val toJSON: Map[String, Any] = Map("vertices" -> vertices.map(_.toJSON), "edges" -> edges, "grrm" -> grrm)

  def findPrefix():String
  def readGRRM():String
  def readEQ():List[EQ]
  def readTS():List[TS]
  def readDC():List[DC]
  def arrange(): Unit = {
    val shuffledEQs: List[EQ] = EQs1.map((_, Random.nextDouble())).sortBy(_._2).map(_._1)
    for (index <- shuffledEQs.indices) {
      val eq: EQ = EQs1(index)
      val x: Double = index % num_grids
      val y: Double = index / num_grids + x % 2 * 0.5
      eq.position = Point(gap.x * (x + 0.5), gap.y * (y + 0.5))
    }
    var i:Int = 0
    while (EQs1.exists(_.arrange()) && i < EQs1.size) {
      i = i + 1
    }
    i = 0
    while (TSs.exists(_.arrange()) && i < TSs.size) {
      i = i + 1
    }
    i = 0
    while (DCs.exists(_.arrange()) && i < DCs.size) {
      i = i + 1
    }
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

    def dist(node: Node): Double = position.dist(node.position)

    def _random_position: Point = Point(origin.x + extent.x * Random.nextDouble(), origin.y + extent.y * Random.nextDouble())

    def arrange(): Boolean = {
      position = (0 until 100)
        .map((_: Int) => Point(origin.x + extent.x * Random.nextDouble(), origin.y + extent.y * Random.nextDouble()))
        .map((p: Point) => (p, (EQs1 ++ TSs ++ DCs)
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

  case class EQ(label: String, energy: Double, geometry: List[List[Any]]=List.empty)
    extends Node(label: String, energy: Double, geometry: List[List[Any]]) {
    var TSs: List[TS] = List()
    var DCs: List[DC] = List()

    def origin: Point = position - gap * 0.5

    def extent: Point = gap

    val isDummy = false

    override def arrange(): Boolean = {
      def total_ts_dist(p: Point, eq: EQ) = eq.TSs.map(_.peer(eq).position.dist(p)).sum
      val self_d: Double = total_ts_dist(position, this)
      val best = EQs1
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

    def origin: Point =
      if (eq1 == eq2)
        eq1.origin
      else
        Point(Math.min(eq1.position.x, eq2.position.x), Math.min(eq1.position.y, eq2.position.y))

    def extent: Point =
      if (eq1 == eq2)
        eq1.extent
      else
        Point(Math.abs(eq1.position.x - eq2.position.x), Math.abs(eq1.position.y - eq2.position.y))

    def dist_between_eqs: Double = eq1.position.dist(eq2.position)

    def peer(eq: EQ): EQ = if (eq == eq1) eq2 else if (eq == eq2) eq1 else null

    val _multiframe: (List[(String, Double)], BigInt) = {
      if (needsMultiframe) {
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
      } else (List.empty, 0)
    }

    override def toJSON: Map[String, Any] = super.toJSON +
      ("multiframe_xyz" -> _multiframe._1.map(_._1)) +
      ("multiframe_energies" -> _multiframe._1.map(_._2)) +
      ("multiframe_index" -> _multiframe._2)
  }

  case class DC(label: String, energy: Double, geometry: List[List[Any]])
    extends Node(label: String, energy: Double, geometry: List[List[Any]]) {
    var eq: EQ = EQ("", 0.0)

    def origin: Point = eq.origin

    def extent: Point = eq.extent

    val _multiframe: List[(String, Double)] = {
      if (needsMultiframe) {
        val chunks = Source.fromFile(prefix + "_" + label + ".log").mkString.split("=========================================================================")
        val header = "" + geometry.length + "\n\n"
        val dc = List((header + chunks(0).split("\nENERGY")(0).split("\n").drop(2).mkString("\n"), energy))
        val frames = chunks(1).split("\n\n").filter(_.startsWith("# STEP "))
        val forward_frames = frames.map((step: String) => (
          header + step.split("\nENERGY")(0).split("\n").drop(1).mkString("\n"),
          step.split("\n").reverse(1).split("=")(1).trim.toDouble)).toList
        forward_frames.reverse ++ dc
      } else (List.empty)
    }

    override def toJSON: Map[String, Any] = super.toJSON +
      ("multiframe_xyz" -> _multiframe.map(_._1)) +
      ("multiframe_energies" -> _multiframe.map(_._2))
  }

  def readBonds(): Unit = {
    val babel = Process("obabel -i xyz - -o mol") // sd
    val babel_io = new ProcessIO(
        allXYZs: ((OutputStream) => Unit),
        collectBonds: ((InputStream) => Unit),
        voidErr: ((InputStream) => Unit))
    (babel run babel_io).exitValue
  }

  def readSmiles(): Unit = {
    val babel = Process("obabel -i xyz - -o can")
    val babel_io = new ProcessIO(
      allXYZs: ((OutputStream) => Unit),
      collectSmiles: ((InputStream) => Unit),
      voidErr: ((InputStream) => Unit))
    (babel run babel_io).exitValue
  }

  def readInchi(): Unit = {
    val babel = Process("obabel -i xyz - -o inchi")
    val babel_io = new ProcessIO(
      allXYZs: ((OutputStream) => Unit),
      collectInchi: ((InputStream) => Unit),
      voidErr: ((InputStream) => Unit))
    (babel run babel_io).exitValue
  }

  def readCanost(): Unit = {
    for (node: Node <- (EQs ++ DCs ++ TSs)) {
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
    def write(geometry: List[List[Any]]): Unit = {
      out.write(("" + (geometry.length) + "\n\n" +
        geometry.map((atom: List[Any]) =>
          atom(0).asInstanceOf[String] + " " +
            atom(1).asInstanceOf[Double] + " " +
            atom(2).asInstanceOf[Double] + " " +
            atom(3).asInstanceOf[Double]).mkString("\n") + "\n").getBytes)
    }
    for (eq: EQ <- EQs)
      write(eq.geometry)
    for (dc: DC <- DCs)
      write(dc.geometry)
    for (ts: TS <- TSs)
      write(ts.geometry)
    out.close()
  }

  def collectBonds(in: InputStream): Unit = {
    val stdout = Source.fromInputStream(in).getLines
    def read(node: Node): Unit = {
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
      while (stdout.hasNext && readline().trim != "$$$$") {}
      node.sdf = lines.reverse.mkString("\n")
    }
    for (eq: EQ <- EQs)
      read(eq)
    for (dc: DC <- DCs)
      read(dc)
    for (ts: TS <- TSs)
      read(ts)
  }

  def collectSmiles(in: InputStream): Unit = {
    val stdout = Source.fromInputStream(in).getLines
    def read(node: Node): Unit = {
      node.smiles = stdout.next().trim.split("\t")(0).trim
    }
    for (eq: EQ <- EQs)
      read(eq)
    for (dc: DC <- DCs)
      read(dc)
    for (ts: TS <- TSs)
      read(ts)
  }

  def collectInchi(in: InputStream): Unit = {
    val stdout = Source.fromInputStream(in).getLines
    def read(node: Node):Unit = {
      var line = stdout.next()
      while (!line.startsWith("InChI="))
        line = stdout.next()
      node.inchi = line.substring(6).trim
    }
    for (eq: EQ <- EQs)
      read(eq)
    for (dc: DC <- DCs)
      read(dc)
    for (ts: TS <- TSs)
      read(ts)
  }

  def voidErr(in: InputStream): Unit = ()

}
