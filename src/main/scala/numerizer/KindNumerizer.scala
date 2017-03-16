package numerizer

import graph.{Vertex, ReactionMap}

/**
 * Created by tomohiro on 15/02/18.
 */
class KindNumerizer(rmap:ReactionMap) extends Numerizer(rmap:ReactionMap) {
  val name:String = "Kind"
  def apply(v:Vertex):Option[Double] = v.label.charAt(0) match {
    case 'D' => Some(0.0)
    case 'T' => Some(1.0)
    case _ => Some(0.5)
  }
  def renew():Numerizer = new KindNumerizer(rmap)

}
