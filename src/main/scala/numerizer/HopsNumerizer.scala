package numerizer

import graph.{Edge, Vertex, ReactionMap}
import scala.collection.mutable

/**
 * Created by tomohiro on 15/02/18.
 */
class HopsNumerizer(rmap:ReactionMap, updater:(Unit=>Unit), setter:(Numerizer=>Unit)) extends Numerizer(rmap:ReactionMap) {
  val hops:mutable.Map[Vertex,Int] = mutable.Map()
  val done:mutable.Set[Vertex] = mutable.Set()
  var maxHop:Int = 0
  def stuff(vertices:Set[Vertex]):Unit = {
    done ++= vertices
    for (v <- vertices)
      hops.update(v, maxHop)
    maxHop += 1
    val newHop:Set[Vertex] =
      vertices.map(
        (v)=>v.edges.toSet[Edge].map((e)=>e.peer(v).getOrElse(v)).filter((v2:Vertex)=> !done.contains(v2))).reduce(_++_)
    if (!newHop.isEmpty) {
      maxHop += 1
      stuff(newHop)
    }
  }
  if (!rmap.reactants.isEmpty)
    stuff(rmap.reactants.toSet)
  val size = rmap.reactants.size

  override def update() = {
    if (hops.isEmpty || rmap.reactants.size != size) {
      setter(transitionTo(this.renew(), updater, setter))
    }
  }
  val name = "Hops"
  def apply(v:Vertex):Option[Double] = {
    if (hops.isEmpty) {
      Some(0.5)
    } else {
      hops.get(v) match {
        case Some(hop) => Some(hop.toDouble / maxHop.toDouble)
        case None => None
      }
    }
  }
  def renew():Numerizer = new HopsNumerizer(rmap, updater, setter)
}
