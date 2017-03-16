package numerizer

import graph.{Vertex, ReactionMap}

/**
  * Created by tomohiro on 2016/02/22.
  */

class SimilarityYNumerizer(rmap:ReactionMap, updater:(Unit=>Unit), setter:(Numerizer=>Unit)) extends Numerizer(rmap:ReactionMap) {
  val selections:List[Vertex] = rmap.selections.toList
  val values:Map[Vertex, Double] =
    if (selections.isEmpty) {
      rmap.vertices.toList.map((vertex:Vertex)=>vertex -> vertex.position.y).toMap[Vertex,Double]
    } else {
      val distances = rmap.vertices.toList.map((vertex: Vertex) =>
        vertex -> selections.map((v: Vertex) => vertex.geometrySignature.dist(v.geometrySignature)).
          min(Ordering.Double)).toMap[Vertex, Double]
      val max = distances.values.max(Ordering.Double)
      if (max > 1.0e-8)
        distances.mapValues(_ / max)
      else
        distances
    }
  val name:String = "Similarity"
  def apply(v:Vertex):Option[Double] = values.get(v)
  def renew():Numerizer = new SimilarityYNumerizer(rmap, updater, setter)

  override def update() = {
    if (selections.length != rmap.selections.size) {
      setter(transitionTo(this.renew(), updater, setter))
    }
  }
}
