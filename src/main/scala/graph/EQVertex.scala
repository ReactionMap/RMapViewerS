package graph

import geometry.XYZFrame

/**
 * Created by tomohiro on 15/02/10.
 */
class EQVertex(json:Map[String, Any]) extends Vertex(json:Map[String, Any]) {
  override val isEQ = true
  def indexFrom(v:Vertex):Int = {
    0
  }
  def indexFromTo(v1:Vertex, v2:Vertex):Int = {
    0
  }
  def indexTo(v:Vertex):Int = {
    0
  }

  def energiesFrom(v:Vertex):List[Double] = List(energy)
  def energiesFromTo(v1:Vertex, v2:Vertex):List[Double] = List(energy)
  def energiesTo(v:Vertex):List[Double] = List(energy)
  def framesFrom(v:Vertex):List[XYZFrame] = List(XYZFrame(geometry))
  def framesFromTo(v1:Vertex, v2:Vertex):List[XYZFrame] = List(XYZFrame(geometry))
  def framesTo(v:Vertex):List[XYZFrame] = List(XYZFrame(geometry))
  def multiframeEnergies:List[Double] = List(energy)
  def multiframeFrames:List[XYZFrame] = List(XYZFrame(geometry))
  def multiframeIndex:Int = 0
  def multiframeCaptions:List[String] = List(label)
}
