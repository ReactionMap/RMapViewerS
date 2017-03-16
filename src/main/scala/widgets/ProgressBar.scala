package widgets

import java.awt.Color

import graph.{GraphAspect, ReactionMap}

import scala.swing._

/**
 * Created by tomohiro on 15/02/19.
 */
case class ProgressBar(rmap:ReactionMap) extends Panel {
  rmap.dependents += ((aspect:GraphAspect)=>if (aspect == GraphAspect.PROGRESS) value = rmap.progress)
  background = Color.WHITE
  ignoreRepaint = false
  preferredSize = new Dimension(700, 20)
  private var _value:Double = 0.0
  def value = _value
  def value_=(v:Double) = {
    _value = v
    peer.paintImmediately(new Rectangle(size))
  }
  def apply(v:Double) = value = v
  override def paint(graphics : Graphics2D): Unit = {
    graphics.setPaint(Color.BLUE)
    graphics.fillRect(1, 1, ((size.getWidth-2)*value).toInt, size.getHeight.toInt-2)
  }
}
