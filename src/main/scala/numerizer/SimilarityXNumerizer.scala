package numerizer

import graph.{Vertex, ReactionMap}

/**
  * Created by tomohiro on 2016/02/22.
  */
class SimilarityXNumerizer(rmap:ReactionMap, updater:(Unit=>Unit), setter:(Numerizer=>Unit)) extends Numerizer(rmap:ReactionMap) {
  val selections:List[Vertex] = rmap.selections.toList
  val values:Map[Vertex, Double] =
    if (selections.isEmpty) {
      rmap.vertices.toList.map((vertex:Vertex)=>vertex -> vertex.position.x).toMap[Vertex,Double]
    } else {
      val distances:Map[Vertex, Double] = rmap.vertices.toList.map((vertex: Vertex) =>
        vertex -> selections.map((v: Vertex) => vertex.geometrySignature.dist(v.geometrySignature)).
          min(Ordering.Double)).toMap[Vertex, Double]
      val sortedVertices = rmap.vertices.toList.sortWith((less, more) => distances(less) <= distances(more))
      (0 until sortedVertices.length).map((index:Int)=>sortedVertices(index)->index.toDouble/sortedVertices.length).toMap[Vertex, Double]
    }
  val name:String = "Similarity Order"
  def apply(v:Vertex):Option[Double] = values.get(v)
  def renew():Numerizer = new SimilarityXNumerizer(rmap, updater, setter)

  override def update() = {
    if (selections.length != rmap.selections.size) {
      setter(transitionTo(this.renew(), updater, setter))
    }
  }

}
