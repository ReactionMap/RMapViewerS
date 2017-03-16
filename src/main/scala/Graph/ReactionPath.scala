package graph

import application.RMapViewer
import geometry.XYZFrame
import net.sourceforge.rmapviewer.rmapjmol.RMapJmol

import scala.collection.mutable

/**
 * Created by tomohiro on 2014/08/21.
 */

class ReactionPath(vs:List[Vertex]) {
  val vertices:List[Vertex] = vs
  val hops: Int = vertices.size
  val energies: List[Double] = vertices.map(_.energy)
  val maxEnergy: Double = energies.reduce(math.max:(Double,Double)=>Double)
  var maxDeltaEnergy: Double = (energies.tail, energies).zipped.map(_ - _).reduce(math.max:(Double,Double)=>Double)
  val label = RMapViewer.mapPanel.searcher.labelFor(this)
  override def toString:String = label
  def openJmol() = {
    vertices.size match {
      case 0 => ()
      case 1 =>
        val v:Vertex = vertices(0)
        new RMapJmol(v.label, XYZFrame(v.geometry).toXYZ, List(v.label).toArray, List(v.energy).toArray)
      case _ =>
        val frames : mutable.MutableList[XYZFrame] = mutable.MutableList()
        val captions : mutable.MutableList[String] = mutable.MutableList()
        val energies : mutable.MutableList[Double] = mutable.MutableList()
        frames ++= vertices(0).framesTo(vertices(1))
        captions += vertices(0).label
        captions ++= (1 until frames.size).map((_)=> "")
        energies ++= vertices(0).energiesTo(vertices(1))
        for (index <- 1 until vertices.size - 1) {
          val from = vertices(index-1)
          val v = vertices(index)
          val to = vertices(index+1)
          val newFrames = v.continueFrames(frames.last, from, to)
          frames ++= newFrames
          val captionIndex = v.indexFromTo(from, to)
          captions ++= (0 until v.indexFromTo(from, to)).map((_) => "")
          captions += v.label
          captions ++= (v.indexFromTo(from, to) + 1 until newFrames.size).map((_) => "")
          energies ++= v.energiesFromTo(from, to)
        }
        val lastFrames = vertices.last.continueFrames(frames.last, vertices(vertices.size-2))
        frames ++= lastFrames
        captions ++= (0 until lastFrames.size-1).map((_)=>"")
        captions += vertices.last.label
        energies ++= vertices.last.energiesFrom(vertices(vertices.size-2))
        new RMapJmol(
          label,
          frames.map(_.toXYZ).reduce(_+_),
          captions.toArray,
          energies.toArray)
    }
  }
}