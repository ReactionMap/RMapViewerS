package numerizer

import graph.{Vertex, ReactionMap}

/**
 * Created by tomohiro on 15/02/18.
 */
class EnergyNumerizer(rmap:ReactionMap) extends Numerizer(rmap:ReactionMap) {
  val offset:Double =
    if (rmap.vertices.isEmpty)
      0.0
    else
      rmap.vertices.map(_.energy).min
  val scale:Double =
    if (rmap.vertices.isEmpty)
      0.0
    else {
      val max = rmap.vertices.map(_.energy).max
      if (Math.abs(max - offset) < 1.0e-8)
        0.0
      else
        1.0 / (max - offset)
    }
  val name:String = "Energy"
  def apply(v:Vertex):Option[Double] = Some((v.energy-offset) * scale)
  def renew():Numerizer = new EnergyNumerizer(rmap)
}
