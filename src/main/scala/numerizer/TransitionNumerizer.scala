package numerizer

import java.util.{TimerTask, Timer}

import graph.{Vertex, ReactionMap}

/**
 * Created by tomohiro on 15/02/18.
 */
class TransitionNumerizer(rmap:ReactionMap, from:Numerizer, to:Numerizer, updater:(Unit=>Unit), setter:(Numerizer=>Unit))
  extends Numerizer(rmap:ReactionMap) {
  val name = "Transition from "+from.name+" to "+to.name
  val maxStep:Int = 10
  var step:Int = 0
  def apply(v:Vertex):Option[Double] = {
    val theta:Double = (Math.PI / 2.0 * step.toDouble / maxStep.toDouble)
    from(v) match {
      case Some(v1) => to(v) match {
        case Some(v2) => Some(v1*Math.cos(theta) + v2*Math.sin(theta))
        case None => None
      }
      case None => None
    }
  }
  def renew():Numerizer = {
    timer.cancel()
    timer.purge()
    new TransitionNumerizer(rmap, from, to, updater, setter)
  }
  val timer:Timer = new Timer()
  timer.schedule(new TimerTask {
    override def run(): Unit = {
      step += 1
      if (step > maxStep) {
        timer.cancel()
        timer.purge()
        setter(to)
      } else {
        updater()
      }
    }
  }, 100, 100)
}
