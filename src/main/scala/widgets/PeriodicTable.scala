package widgets

import java.awt.{Font, BasicStroke, Color, Rectangle}

import atom.Atom

import scala.swing._
import scala.swing.event.MouseClicked

/**
  * Created by tomohiro on 2016/02/23.
  */
class PeriodicTable extends Panel {
  private var _value:Atom = Atom.C
  def value = _value
  def value_=(v:Atom) = {
    _value = v
    peer.paintImmediately(new Rectangle(size))
  }
  override def paint(graphics : Graphics2D): Unit = {
    val font:Font = new Font("Arial", Font.BOLD, 16)
    graphics.setFont(font)
    val fontMetrics = graphics.getFontMetrics
    for (atom:Atom <- Atom.atoms) {
      val left = (atom.group - 1) * size.width / 18
      val top = (atom.period - 1) * size.height / 5
      val right = atom.group * size.width/18
      val bottom = atom.period * size.height/5
      graphics.setPaint(atom.color)
      graphics.setStroke(new BasicStroke(1))
      graphics.fillRect(left, top, right-left, bottom-top)
      graphics.setPaint(Color.BLACK)
      graphics.drawString(
        atom.symbol,
        left+(size.width/18-fontMetrics.stringWidth(atom.symbol))/2,
        bottom-(size.height/5-fontMetrics.getAscent())/2)
      if (_value == atom) {
        graphics.setStroke(new BasicStroke(4))
        graphics.drawLine(left+2, top+2, right-2, top+2)
        graphics.drawLine(left+2, top+2, left+2, bottom-2)
        graphics.drawLine(left+2, bottom-2, right-2, bottom-2)
        graphics.drawLine(right-2, top+2, right-2, bottom-2)
      }
    }
  }
  listenTo(mouse.clicks, mouse.moves, mouse.wheel)
  reactions += {
    case event: MouseClicked => {
      val group = event.point.x / (size.width / 18) + 1
      val period = event.point.y / (size.height / 5) + 1
      value = Atom(group, period) match {
        case Some(atom) => atom
        case None => value
      }
    }
  }
  preferredSize = new Dimension(400, 200)
  maximumSize = new Dimension(800,200)
}
