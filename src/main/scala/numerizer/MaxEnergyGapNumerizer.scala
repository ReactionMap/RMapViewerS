package numerizer

import graph.{ReactionMap, Vertex}

import scala.collection.mutable

/**
 * Created by tomohiro on 15/02/18.
 */
class MaxEnergyGapNumerizer(rmap:ReactionMap, updater:(Unit=>Unit), setter:(Numerizer=>Unit)) extends Numerizer(rmap:ReactionMap) {

  var gaps:mutable.Map[Vertex, Double] = mutable.Map()
  var reactants:Set[Vertex] = rmap.reactants.toSet
  if (true) {
    val newGaps:mutable.Map[Vertex, Double] = mutable.Map()
    def stuff(vertices:Map[Vertex, Double]):Unit = {
      newGaps ++= vertices
      val nextGaps:mutable.Map[Vertex, Double] = mutable.Map()
      for (vertex <- vertices.keys) {
        for (edge <- vertex.edges) {
          edge.peer(vertex) match {
            case Some(newVertex) =>
              val gap = Math.max(newVertex.energy - vertex.energy, vertices(vertex))
              if (!newGaps.contains(newVertex) || newGaps(newVertex) > gap) {
                newGaps.update(newVertex, gap)
                nextGaps.update(newVertex, gap)
              }
            case None => ()
          }
        }
      }
      if (!nextGaps.isEmpty) {
        stuff(nextGaps.toMap)
      }
    }
    if (!rmap.reactants.isEmpty)
      stuff(rmap.reactants.map((v)=>(v, 0.0)).toMap[Vertex,Double])
    gaps = newGaps
  }
  val maxGap = if (gaps.isEmpty) 1.0 else gaps.values.max

  override def update() = {
    if (gaps.isEmpty || rmap.reactants.size != reactants.size) {
      setter(transitionTo(this.renew(), updater, setter))
    }
  }
  val name = "Max Energy Gap"
  def apply(v:Vertex):Option[Double] = {
    if (gaps.isEmpty) {
      Some(0.5)
    } else {
      gaps.get(v) match {
        case Some(gap) => Some(gap/maxGap)
        case None => None
      }
    }
  }
  def renew():Numerizer = new MaxEnergyGapNumerizer(rmap, updater, setter)
}
