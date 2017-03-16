package graph.searcher

import graph.{ReactionMap, ReactionPath, Vertex}

import scala.collection.mutable

/**
 * Created by tomohiro on 15/02/19.
 */
class EnergyShortestPathSearcher (rmap:ReactionMap, shortestOnly:Boolean)
  extends ShortestPathSearcher(rmap:ReactionMap, shortestOnly:Boolean) {
  override val basename = "Energy"
  def energies:Array[Double] = rmap.vertices.map((v)=>v.energy).toList.sorted.toArray
  override def labelFor(path:ReactionPath) = {
    var hotVertex = path.vertices.head
    var gap = 0.0
    path.vertices.foreach((v:Vertex) => {
      val g = v.energy - path.vertices.head.energy
      if (g >= gap) {
        gap = g
        hotVertex = v
      }
    })
    val gap10 = ((gap * 2625.49962) * 10.0).round
    (gap10 / 10).toString + "." + (gap10 % 10).toString() + " KJ/mol : "+ path.vertices.map((v:Vertex) =>
      (if (v == hotVertex)"["+v.label+"]" else v.label)).reduce(_+"->"+_)
  }

  def connectionsWithin(energy:Double):Map[Vertex, List[Vertex]] = {
    val connections:mutable.Map[Vertex, List[Vertex]] = mutable.Map()
    for (edge <- rmap.edges) {
      if (edge.vertex2.energy <= energy)
        connections.update(edge.vertex1, edge.vertex2::connections.getOrElse(edge.vertex1, List()))
      if (edge.vertex1.energy <= energy)
        connections.update(edge.vertex2, edge.vertex1::connections.getOrElse(edge.vertex2, List()))
    }
    connections.toMap
  }
}
