package graph

/**
 * Created by tomohiro on 15/02/19.
 */
sealed abstract class GraphAspect(code:String) {
  val name = toString
}
object GraphAspect {
  case object PATHS extends GraphAspect("paths")
  case object PATH extends GraphAspect("path")
  case object MAP extends GraphAspect("map")
  case object RENDERING extends GraphAspect("rendering")
  case object SELECTION extends GraphAspect("selection")
  case object PROGRESS extends GraphAspect("progress")
}
