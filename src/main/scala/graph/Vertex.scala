package graph

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color}

import atom.Atom
import geometry._
import net.sourceforge.rmapviewer.rmapjmol.RMapJmol

import scala.collection.mutable
import scala.swing.{Swing, Dialog}

/**
 * Created by tomohiro on 2014/07/31.
 */
abstract case class Vertex(json:Map[String, Any]) {
  val edges: mutable.MutableList[Edge] = new mutable.MutableList[Edge]()
  val label: String = json("label").asInstanceOf[String]
  val position: Point = Point(json("position").asInstanceOf[List[Double]](0),
                                  json("position").asInstanceOf[List[Double]](1))
  val bonds : List[(Int, Int, Int)] =
    json("bonds").asInstanceOf[List[List[Any]]]
      .map((triple)=>(
        triple(0).asInstanceOf[BigInt].intValue(),
        triple(1).asInstanceOf[BigInt].intValue(),
        triple(2).asInstanceOf[BigInt].intValue()))
  val geometry:List[List[Any]] =
    json.get("geometry") match {
      case Some(xyz:List[List[Any]]) => xyz
      case _ => List()
    }
  def smiles:String = json.getOrElse("smiles", "N/A").asInstanceOf[String]
  def inchi:String = json.getOrElse("inchi", "N/A").asInstanceOf[String]
  def canost:String = json.getOrElse("canost", "N/A").asInstanceOf[String]
  val geometrySignature:GeometrySignature = new GeometrySignature(geometry)
  val structureSignature:StructureSignature = new StructureSignature(geometry, bonds)

  val energy: Double = json("energy").asInstanceOf[Double]
  var baselineEnergy: Double = energy
  def kJmol:Double = (energy-baselineEnergy) * 2625.5
  def kJmolString = BigDecimal(kJmol).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString+"kJ/mol"
  def apply(key:String):Any = {
    json(key)
  }
  def apply(key:String, default:Any):Any = {
    json.getOrElse(key, default)
  }
  def get[T](key:String):Option[T] = {
    json.get(key).asInstanceOf[Option[T]]
  }
  val isDC:Boolean = false
  val isTS:Boolean = false
  val isEQ:Boolean = false

  def openJmol():RMapJmol = {
    new RMapJmol(
      this.label,
      this.multiframeFrames.map(_.toXYZ).reduce(_+_),
      this.multiframeCaptions.toArray,
      this.multiframeEnergies.toArray)
  }

  def showInfo():Unit = {
    var msg = List(label, "Energy:"+kJmolString, "SMILES:"+smiles, "InChI:"+inchi, "CAST-1D:"+canost).mkString("\n")
    Dialog.showMessage(null, msg, "RMapViewer:"+label, Dialog.Message.Plain, Swing.Icon(image))
  }

  def continueFrames(frame:XYZFrame, from:Vertex, to:Vertex) = {
    val frames = framesFromTo(from, to)
    val transformation = frame.transformation(frames.head)
    frames.map(_.transformedBy(transformation))
  }
  def continueFrames(frame:XYZFrame, from:Vertex) = {
    val frames = framesFrom(from)
    val transformation = frame.transformation(frames.head)
    frames.map(_.transformedBy(transformation))
  }

  def indexFrom(v:Vertex):Int
  def indexFromTo(v1:Vertex, v2:Vertex):Int
  def indexTo(v:Vertex):Int
  def energiesFrom(v:Vertex):List[Double]
  def energiesFromTo(v1:Vertex, v2:Vertex):List[Double]
  def energiesTo(v:Vertex):List[Double]
  def framesFrom(v:Vertex):List[XYZFrame]
  def framesFromTo(v1:Vertex, v2:Vertex):List[XYZFrame]
  def framesTo(v:Vertex):List[XYZFrame]
  def multiframeEnergies:List[Double]
  def multiframeFrames:List[XYZFrame]
  def multiframeIndex:Int
  def multiframeCaptions:List[String]
  val image:BufferedImage = createImage()
  def createImage(imageExtent:Int=64):BufferedImage = {
    val xs = geometry.map(quad=>quad(1).asInstanceOf[Double])
    val ys = geometry.map(quad=>quad(2).asInstanceOf[Double])
    val zs = geometry.map(quad=>quad(3).asInstanceOf[Double])
    val coords = List(xs.max - xs.min, ys.max - ys.min, zs.max - zs.min)
    val trans = coords.indices.sortBy(coords(_))
    val geom = geometry.map(quad=>List(quad(0), quad(trans(2)+1), quad(trans(1)+1), quad(trans(0)+1)))
    val image = new BufferedImage(imageExtent, imageExtent, BufferedImage.TYPE_INT_ARGB)
    val graphics = image.createGraphics()
    val zSortedIndices = geom.indices.sortWith(
      (more:Int, less:Int)=>
        geom(less)(3).asInstanceOf[Double] <= geom(more)(3).asInstanceOf[Double])
    val minX = geom.map(_(1).asInstanceOf[Double]).min - 1.0
    val maxX = geom.map(_(1).asInstanceOf[Double]).max + 1.0
    val centerX = (minX + maxX) / 2.0
    val minY = geom.map(_(2).asInstanceOf[Double]).min - 1.0
    val maxY = geom.map(_(2).asInstanceOf[Double]).max + 1.0
    val centerY = (minY + maxY) / 2.0
    val minZ = geom.map(_(3).asInstanceOf[Double]).min - 1.0
    val maxZ = geom.map(_(3).asInstanceOf[Double]).max + 1.0
    val centerZ = (minZ + maxZ) / 2.0
    val scale = imageExtent / Math.max(maxX-minX, maxY-minY)
    def normalize(x:Double, y:Double, z:Double) =
      (((x - centerX) * scale + imageExtent/2.0).toInt,
        ((y - centerY) * scale + imageExtent/2.0).toInt)
    graphics.setPaint(new Color(0, 0, 0, 0))
    graphics.fillRect(0,0,imageExtent,imageExtent)
    val done:mutable.Set[Int] = mutable.Set[Int]()
    for (index <- zSortedIndices) {
      done.add(index)
      var quadruple = geom(index)
      val atom:Atom = Atom(quadruple.head.asInstanceOf[String]).get
      val (x, y) = normalize(
        quadruple(1).asInstanceOf[Double],
        quadruple(2).asInstanceOf[Double],
        quadruple(3).asInstanceOf[Double])
      for (triple <- bonds) {
        if (triple._1 == index && !done.contains(triple._2)) {
          val (x1, y1) = normalize(
            geom(triple._2)(1).asInstanceOf[Double],
            geom(triple._2)(2).asInstanceOf[Double],
            geom(triple._2)(3).asInstanceOf[Double])
          graphics.setPaint(Color.BLACK)
          graphics.setStroke(new BasicStroke(3))
          graphics.drawLine(x, y, x1, y1)
        }
        if (triple._2 == index && !done.contains(triple._1)) {
          val (x1, y1) = normalize(
            geom(triple._1)(1).asInstanceOf[Double],
            geom(triple._1)(2).asInstanceOf[Double],
            geom(triple._1)(3).asInstanceOf[Double])
          graphics.setPaint(Color.BLACK)
          graphics.setStroke(new BasicStroke(3))
          graphics.drawLine(x, y, x1, y1)
        }
      }
      val radius = atom.vanDerWaalsRadius * scale / 4.0
      graphics.setStroke(new BasicStroke(1))
      graphics.setPaint(atom.color)
      graphics.fillOval((x-radius).toInt, (y-radius).toInt, (radius*2.0).toInt, (radius*2.0).toInt)
      graphics.setPaint(Color.BLACK)
      graphics.drawOval((x-radius).toInt, (y-radius).toInt, (radius*2.0).toInt, (radius*2.0).toInt)
    }
    image
  }
}
object Vertex {
  def apply(json:Map[String, Any]):Vertex = {
    json("label").asInstanceOf[String].charAt(0) match {
      case 'D' => new DCVertex(json)
      case 'T' => new TSVertex(json)
      case 'E' => new EQVertex(json)
    }
  }
}