package graph.searcher

import graph.{ReactionMap, ReactionPath, Vertex}

import scala.collection.mutable

/**
 * Created by tomohiro on 15/02/19.
 */
abstract class ShortestPathSearcher(rmap:ReactionMap, shortestOnly:Boolean) {
  val basename:String
  def name = basename + (if (shortestOnly) "(shortest only)" else "(shortest+2hops)")
  def search():List[List[Vertex]] = {
    if (rmap.reactants.isEmpty || rmap.products.isEmpty)
      return List()
    rmap.progress = 0.25
    val maybeEnergy:Option[Double] = thresholdEnergy()
    rmap.progress = 0.5
    maybeEnergy match {
      case None =>
        rmap.progress = 0.0
        List()
      case Some(energy) =>
        val result:List[List[Vertex]] =
          if (shortestOnly)
            allShortestPaths(connectionsWithin(energy))
          else
            allPaths(connectionsWithin(energy))
        rmap.progress = 0.0
        result
    }
  }

  def thresholdEnergy():Option[Double] = {
    val energies = this.energies
    var min:Int = 0
    var max:Int = energies.size - 1
    while (min != max) {
      val index:Int = (min + max + 1) / 2
      if (isReachable(connectionsWithin(energies(index)))) {
        max = index - 1
      } else {
        min = index
      }
    }
    if (min+1 < energies.size)
      Some(energies(min + 1))
    else
      None
  }

  def allPaths(connection:Map[Vertex, List[Vertex]]):List[List[Vertex]] = {
    var agenda:List[List[Vertex]] = rmap.reactants.map((v)=>List(v)).toList
    var paths:List[List[Vertex]] = List()
    var hops = connection.size
    while (agenda.nonEmpty) {
      var newAgenda: List[List[Vertex]] = List()
      var newDone: List[Vertex] = List()
      for (path <- agenda) {
        for (peer <- connection.getOrElse(path.head, List())) {
          if (!path.contains(peer)) {
            val newPath = peer::path
            if (rmap.products.contains(peer)) {
              paths = newPath::paths
              if (hops > newPath.length+3) {
                hops = newPath.length+3
              }
            } else if (newPath.length <= hops) {
              newAgenda = newPath::newAgenda
            }
          }
        }
      }
      agenda = newAgenda
      rmap.progress = 1.0-(1.0-rmap.progress)/2
    }
    paths.reverseMap(_.reverse)
  }

  def allShortestPaths(connection:Map[Vertex, List[Vertex]]):List[List[Vertex]] = {
    val done:mutable.Set[Vertex] = mutable.Set() ++ rmap.reactants
    var agenda:List[List[Vertex]] = rmap.reactants.map((v)=>List(v)).toList
    var paths:List[List[Vertex]] = List()
    while (paths.isEmpty && agenda.nonEmpty) {
      var newAgenda: List[List[Vertex]] = List()
      var newDone: List[Vertex] = List()
      for (path <- agenda) {
        for (peer <- connection.getOrElse(path.head, List())) {
          if (!done.contains(peer)) {
            newDone = peer::newDone
            val newPath = peer::path
            if (rmap.products.contains(peer)) {
              paths = newPath::paths
            } else {
              newAgenda = newPath::newAgenda
            }
          }
        }
      }
      agenda = newAgenda
      done ++= newDone
      rmap.progress = 1.0-(1.0-rmap.progress)/2
    }
    paths.reverseMap(_.reverse)
  }

  def isReachable(connection:Map[Vertex, List[Vertex]]):Boolean = {
    var agenda:List[Vertex] = rmap.reactants.toList
    val done:mutable.Set[Vertex] = mutable.Set()
    while (agenda.nonEmpty) {
      var newAgenda:List[Vertex] = List()
      for (vertex <- agenda) {
        for (peer <- connection.getOrElse(vertex, List())) {
          if (rmap.products.contains(peer)) {
            return true
          }
          if (!done.contains(peer)) {
            done += peer
            newAgenda = peer::newAgenda
          }
        }
      }
      agenda = newAgenda
    }
    false
  }

  def labelFor(path:ReactionPath):String = {
    path.vertices.map(_.label).reduce(_+"->"+_)
  }

  def connectionsWithin(energy:Double):Map[Vertex, List[Vertex]]
  def energies:Array[Double]
}
