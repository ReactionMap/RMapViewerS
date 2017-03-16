package graph

/**
 * Created by tomohiro on 2014/07/31.
 */
case class Edge(vertex1: Vertex, vertex2: Vertex, json: Map[String, BigInt]) {
  vertex1.edges += this
  vertex2.edges += this
  def peer(vertex:Vertex) : Option[Vertex] = {
    if (vertex1 == vertex)
      Some(vertex2)
    else if (vertex2 == vertex)
      Some(vertex1)
    else
      None
  }
}
