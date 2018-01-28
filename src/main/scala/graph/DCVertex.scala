package graph

import geometry.XYZFrame
import net.sourceforge.rmapviewer.rmapjmol.RMapJmol

/**
 * Created by tomohiro on 15/02/10.
 */
class DCVertex(json:Map[String, Any]) extends Vertex(json:Map[String, Any]) {
  override val isDC = true

  override def openJmol():RMapJmol = {
    val jmol = super.openJmol()
    jmol.nextStructure()
    jmol
  }

  def indexFrom(v:Vertex):Int = {
    if (edges.exists((edge)=> Some(v) == edge.peer(this))) {
      json("multiframe_xyz").asInstanceOf[List[String]].size - 1
    } else {
      0
    }
  }
  def indexFromTo(v1:Vertex, v2:Vertex):Int = {
    if (edges.exists((edge)=> Some(v1) == edge.peer(this) && Some(v2) == edge.peer(this))) {
      json("multiframe_xyz").asInstanceOf[List[String]].size - 1
    } else {
      0
    }
  }
  def indexTo(v:Vertex):Int = {
    0
  }

  def energiesFrom(v:Vertex):List[Double] = {
    if (edges.exists((edge)=> Some(v) == edge.peer(this))) {
      json("multiframe_energies").asInstanceOf[List[Double]]
    } else {
      List(energy)
    }
  }
  def energiesFromTo(v1:Vertex, v2:Vertex):List[Double] = {
    if (edges.exists((edge)=> Some(v1) == edge.peer(this)) && edges.exists((edge)=> Some(v2) == edge.peer(this))) {
      val energies = json("multiframe_energies").asInstanceOf[List[Double]]
      energies ++ energies.reverse.tail
    } else {
      List(energy)
    }
  }
  def energiesTo(v:Vertex) :List[Double] = {
    if (edges.exists((edge)=> Some(v) == edge.peer(this))) {
      json("multiframe_energies").asInstanceOf[List[Double]].reverse
    } else {
      List(energy)
    }
  }
  def framesFrom(v:Vertex):List[XYZFrame] = {
    if (edges.exists((edge) => Some(v) == edge.peer(this))) {
      json("multiframe_xyz").asInstanceOf[List[String]].map((xyz)=>XYZFrame(xyz))
    } else {
      List(XYZFrame(geometry))
    }
  }
  def framesFromTo(v1:Vertex, v2:Vertex):List[XYZFrame] = {
    if (edges.exists((edge)=> Some(v1) == edge.peer(this)) && edges.exists((edge)=> Some(v2) == edge.peer(this))) {
      val xyzs = json("multiframe_xyz").asInstanceOf[List[String]]
      xyzs.map((xyz)=>XYZFrame(xyz)) ++ xyzs.reverse.tail.map((xyz)=>XYZFrame(xyz))
    } else {
      List(XYZFrame(geometry))
    }
  }
  def framesTo(v:Vertex):List[XYZFrame] = {
    if (edges.exists((edge)=> Some(v) == edge.peer(this))) {
      json("multiframe_xyz").asInstanceOf[List[String]].reverse.map((xyz)=>XYZFrame(xyz))
    } else {
      List(XYZFrame(geometry))
    }
  }
  def multiframeEnergies:List[Double] = {
    json("multiframe_energies").asInstanceOf[List[Double]]
  }
  def multiframeFrames:List[XYZFrame] = {
    json("multiframe_xyz").asInstanceOf[List[String]].map((xyz)=> XYZFrame(xyz))
  }
  def multiframeIndex:Int = {
    json("multiframe_index").asInstanceOf[Int]
  }
  def multiframeCaptions:List[String] = {
    val l1:String = edges.head.peer(this) match {
      case Some(v: Vertex) => v.label
      case _ => ""
    }
    List(l1) ++ (2 until multiframeEnergies.length).map((_)=>"")++List(label)
  }
}
