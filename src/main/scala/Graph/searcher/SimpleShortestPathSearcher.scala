package graph.searcher

import graph.{ReactionMap, ReactionPath, Vertex}

import scala.collection.mutable

/**
 * Created by tomohiro on 15/02/19.
 */
class SimpleShortestPathSearcher (rmap:ReactionMap, shortestOnly:Boolean)
  extends ShortestPathSearcher(rmap:ReactionMap, shortestOnly:Boolean) {
  override val basename:String = "Shortest"
  override def thresholdEnergy():Option[Double] = Some(0.0)
  def energies:Array[Double] = rmap.vertices.map((v)=>v.energy).toList.sorted.toArray

  override def labelFor(path:ReactionPath) = {
    (path.vertices.filter(_.isEQ).length-1).toString + " : "+super.labelFor(path)
  }

  def connectionsWithin(energy:Double):Map[Vertex, List[Vertex]] = {
    val connections:mutable.Map[Vertex, List[Vertex]] = mutable.Map()
    for (edge <- rmap.edges) {
      connections.update(edge.vertex1, edge.vertex2::connections.getOrElse(edge.vertex1, List()))
      connections.update(edge.vertex2, edge.vertex1::connections.getOrElse(edge.vertex2, List()))
    }
    connections.toMap
  }
}
