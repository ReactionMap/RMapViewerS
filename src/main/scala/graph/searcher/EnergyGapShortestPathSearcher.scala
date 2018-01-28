package graph.searcher

import graph.{ReactionMap, ReactionPath, Vertex}

import scala.collection.mutable

/**
 * Created by tomohiro on 15/02/19.
 */
class EnergyGapShortestPathSearcher(rmap:ReactionMap, shortestOnly:Boolean)
  extends ShortestPathSearcher(rmap:ReactionMap, shortestOnly:Boolean) {
  override val basename:String = "Energy Gap"
  def energies:Array[Double] = rmap.edges.map((edge)=>Math.abs(edge.vertex1.energy-edge.vertex2.energy)).toList.sorted.toArray
  override def labelFor(path:ReactionPath) = {
    var hotVertex = path.vertices.head
    var gap = 0.0
    (0 until path.vertices.length-1).foreach((i:Int) => {
      val g = path.vertices(i+1).energy - path.vertices(i).energy
      if (g >= gap) {
        gap = g
        hotVertex = path.vertices(i+1)
      }
    })
    val gap10 = ((gap * 2625.49962) * 10.0).round
    (gap10 / 10).toString + "." + (gap10 % 10).toString() + " KJ/mol : "+ path.vertices.map((v:Vertex) =>
      (if (v == hotVertex)"["+v.label+"]" else v.label)).reduce(_+"->"+_)
  }

  def connectionsWithin(energy:Double):Map[Vertex, List[Vertex]] = {
    val connections:mutable.Map[Vertex, List[Vertex]] = mutable.Map()
    for (edge <- rmap.edges) {
      if (edge.vertex2.energy - edge.vertex1.energy <= energy)
        connections.update(edge.vertex1, edge.vertex2::connections.getOrElse(edge.vertex1, List()))
      if (edge.vertex1.energy - edge.vertex2.energy <= energy)
        connections.update(edge.vertex2, edge.vertex1::connections.getOrElse(edge.vertex2, List()))
    }
    connections.toMap
  }
}
