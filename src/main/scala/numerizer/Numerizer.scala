package numerizer

import graph.{Vertex, ReactionMap}

/**
 * Created by tomohiro on 15/02/18.
 */
abstract case class Numerizer(rmap: ReactionMap) {
  val name:String
  def update() = ()
  def apply(vertex:Vertex):Option[Double]
  def renew():Numerizer
  def transitionTo(to:Numerizer, updater:(Unit=>Unit), setter:(Numerizer=>Unit)): TransitionNumerizer =
    new TransitionNumerizer(rmap, this, to, updater, setter)
}
