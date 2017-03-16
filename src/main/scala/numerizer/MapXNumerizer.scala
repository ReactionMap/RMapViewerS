package numerizer

import graph.{ReactionMap, Vertex}

/**
 * Created by tomohiro on 15/02/18.
 */
class MapXNumerizer(rmap:ReactionMap) extends Numerizer(rmap:ReactionMap) {
  val offset:Double =
    if (rmap.vertices.isEmpty)
      0.0
    else
      rmap.vertices.map(_.position.x).min
  val scale:Double =
    if (rmap.vertices.isEmpty)
      0.0
    else {
      val max = rmap.vertices.map(_.position.x).max
      if (Math.abs(max - offset) < 1.0e-8)
        0
      else
        1.0 / (max - offset)
    }
  val name:String = "Map X"
  def apply(v:Vertex):Option[Double] = Some((v.position.x-offset) * scale)
  def renew():Numerizer = new MapXNumerizer(rmap)
}
