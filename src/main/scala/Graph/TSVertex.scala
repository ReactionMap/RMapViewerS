package graph

import geometry.XYZFrame

/**
 * Created by tomohiro on 15/02/10.
 */
class TSVertex(json:Map[String, Any]) extends Vertex(json:Map[String, Any]) {
  override val isTS = true

  override def openJmol() = {
    val jmol = super.openJmol()
    jmol.nextStructure()
    jmol
  }

  def indexFrom(v:Vertex):Int = {
    val index:Int = json("multiframe_index").asInstanceOf[BigInt].intValue()
    edges.find((edge)=>Some(v) == edge.peer(this)) match {
      case Some(edge1:Edge) => edges.find((edge)=> edge != edge1) match {
        case Some(edge2:Edge) => edge2.peer(this) match {
          case Some(v2:Vertex) =>
            if (v.label <= v2.label) {
              index
            } else {
              json("multiframe_xyz").asInstanceOf[List[String]].size - index - 1
            }
          case _ => 0
        }
        case _ => 0
      }
      case _ => 0
    }
  }
  def indexFromTo(v1:Vertex, v2:Vertex):Int = {
    val index:Int = json("multiframe_index").asInstanceOf[BigInt].intValue()
    if (edges.exists((edge)=> Some(v1) == edge.peer(this)) &&
      edges.exists((edge)=> Some(v2) == edge.peer(this))) {
      if (v1.label <= v2.label) {
        index
      } else {
        multiframeEnergies.size - index - 1
      }
    } else {
      0
    }
  }
  def indexTo(v:Vertex):Int = {
    val index:Int = json("multiframe_index").asInstanceOf[BigInt].intValue()
    edges.find((edge)=> Some(v) == edge.peer(this)) match {
      case None => 0
      case Some(edge1) => edges.find((edge) => edge != edge1) match {
        case None => 0
        case Some(edge2) => edge2.peer(this) match {
          case None => 0
          case Some(v2:Vertex) =>
            if (v2.label <= v.label) {
              index
            } else {
              json("multiframe_xyz").asInstanceOf[List[String]].size - index - 1
            }
        }
      }
    }
  }
  def energiesFrom(v:Vertex):List[Double] = {
    edges.find((edge)=>Some(v) == edge.peer(this)) match {
      case Some(edge1:Edge) => edges.find((edge)=> edge != edge1) match {
        case Some(edge2:Edge) => edge2.peer(this) match {
          case Some(v2:Vertex) =>
            val energies = multiframeEnergies
            if (v.label <= v2.label) {
              energies.take(Math.min(multiframeIndex, energies.size))
            } else {
              energies.reverse.take(energies.size-multiframeIndex)
            }
          case _ => List(energy)
        }
        case _ => List(energy)
      }
      case _ => List(energy)
    }
  }
  def energiesFromTo(v1:Vertex, v2:Vertex):List[Double] = {
    if (edges.exists((edge)=> Some(v1) == edge.peer(this)) &&
        edges.exists((edge)=> Some(v2) == edge.peer(this))) {
      if (v1.label <= v2.label) {
        multiframeEnergies
      } else {
        multiframeEnergies.reverse
      }
    } else {
      List(energy)
    }
  }
  def energiesTo(v:Vertex):List[Double] = {
    edges.find((edge)=> Some(v) == edge.peer(this)) match {
      case None => List(energy)
      case Some(edge1) => edges.find((edge) => edge != edge1) match {
        case None => List(energy)
        case Some(edge2) => edge2.peer(this) match {
          case None => List(energy)
          case Some(v2:Vertex) =>
            val energies:List[Double] = multiframeEnergies
            if (v2.label <= v.label) {
              energies.drop(multiframeIndex)
            } else {
              energies.reverse.take(Math.min(multiframeIndex + 1, energies.size))
            }
        }
      }
    }
  }



  def framesFrom(v:Vertex):List[XYZFrame] = {
    edges.find((edge)=>Some(v) == edge.peer(this)) match {
      case Some(edge1:Edge) => edges.find((edge)=> edge != edge1) match {
        case Some(edge2:Edge) => edge2.peer(this) match {
          case Some(v2:Vertex) =>
            val frames = multiframeFrames
            if (v.label <= v2.label) {
              frames.take(Math.min(multiframeIndex, frames.size))
            } else {
              frames.reverse.take(frames.size-multiframeIndex)
            }
          case _ => List(XYZFrame(geometry))
        }
        case _ => List(XYZFrame(geometry))
      }
      case _ => List(XYZFrame(geometry))
    }
  }
  def framesFromTo(v1:Vertex, v2:Vertex):List[XYZFrame] = {
    if (edges.exists((edge)=> Some(v1) == edge.peer(this)) &&
      edges.exists((edge)=> Some(v2) == edge.peer(this))) {
      if (v1.label <= v2.label) {
        multiframeFrames
      } else {
        multiframeFrames.reverse
      }
    } else {
      List(XYZFrame(geometry))
    }
  }
  def framesTo(v:Vertex):List[XYZFrame] = {
    edges.find((edge)=> Some(v) == edge.peer(this)) match {
      case None => List(XYZFrame(geometry))
      case Some(edge1) => edges.find((edge) => edge != edge1) match {
        case None => List(XYZFrame(geometry))
        case Some(edge2) => edge2.peer(this) match {
          case None => List(XYZFrame(geometry))
          case Some(v2:Vertex) =>
            val frames:List[XYZFrame] = multiframeFrames
            if (v2.label <= v.label) {
              frames.drop(multiframeIndex)
            } else {
              frames.reverse.take(Math.min(multiframeIndex + 1, frames.size))
            }
        }
      }
    }
  }

  def multiframeEnergies:List[Double] = {
    json("multiframe_energies").asInstanceOf[List[Double]]
  }
  def multiframeFrames:List[XYZFrame] = {
    json("multiframe_xyz").asInstanceOf[List[String]].map((xyz)=> XYZFrame(xyz))
  }
  def multiframeIndex:Int = {
    json("multiframe_index").asInstanceOf[BigInt].intValue()
  }
  def multiframeCaptions:List[String] = {
    val l1:String = if (edges.nonEmpty) {
      edges.head.peer(this) match {
        case Some(v1:Vertex) => v1.label
        case _ => ""
      }
    } else {
      ""
    }
    val l2:String = if (edges.size >= 2) {
      edges(1).peer(this) match {
        case Some(v1:Vertex) => v1.label
        case _ => ""
      }
    } else {
      ""
    }
    val index = multiframeIndex
    List(if (l1 <= l2) l1 else l2) ++
      (1 until index).map((_)=>"") ++
      List(label) ++
      (index+1 until multiframeEnergies.size-1).map((_)=>"") ++
      List(if (l1 <= l2) l2 else l1)
  }
}
