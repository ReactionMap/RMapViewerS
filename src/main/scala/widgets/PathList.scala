package widgets

/**
 * Created by tomohiro on 2014/08/21.
 */

import graph.{GraphAspect, ReactionMap, ReactionPath}

import scala.swing._
import scala.swing.event._

case class PathList(rmap:ReactionMap) extends ListView[String] {
  rmap.dependents += ((aspect:GraphAspect)=>if (aspect == GraphAspect.PATHS) this.update())
  def update() = {
    listData = List[String]()
    listData ++= rmap.paths.map(_.label)
  }
  object Command extends Publisher {
    reactions += {
      case ListSelectionChanged(source, range, live) =>
        val indices = source.selection.indices
        if (indices.isEmpty) {
          rmap.path = None
        } else {
          rmap.path = Some(rmap.paths(indices.head))
        }
    }
    listenTo(mouse.clicks)
    reactions += {
      case e: MouseClicked =>
        if (e.peer.getButton() > 1)
          rmap.path match {
            case Some(path:ReactionPath) => path.openJmol()
            case _ => ()
          }
    }
  }
  Command.listenTo(selection)
}
