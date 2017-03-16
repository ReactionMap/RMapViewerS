package widgets

import java.awt.{BasicStroke, Color}

import graph._

import scala.collection.mutable
import scala.swing._
import scala.swing.event._

/**
 * Created by tomohiro on 2014/07/31.
 */
case class EnergyTreePanel(rmap : ReactionMap) extends Panel {
  background = Color.WHITE
  rmap.dependents += ((aspect:GraphAspect)=>update(aspect))
  preferredSize = new Dimension(400, 350)
  private var last_label_width:Int = 0
  private var last_label_height:Int = 0

  listenTo(mouse.clicks, mouse.moves, mouse.wheel)
  reactions += {
    case event: MouseClicked =>
      hoverVertex(event.point.x, event.point.y) match {
      case Some(vertex) => popupMenu(vertex).show(peer, event.point.x, event.point.y)
      case None => ()
    }
  }

  def popupMenu(vertex: Vertex):javax.swing.JPopupMenu = {
    val menu: javax.swing.JPopupMenu = new javax.swing.JPopupMenu
    if (rmap.isReactant(vertex) || rmap.isProduct(vertex) || rmap.isSelected(vertex)) {
      addMenuItem(menu, "Unselect", ()=> {
        rmap.unselect(vertex)
      })
    }
    if (!rmap.isReactant(vertex)) {
      addMenuItem(menu, "Reactant", ()=> {
        rmap.addReactant(vertex)
      })
    }
    if (!rmap.isProduct(vertex)) {
      addMenuItem(menu, "Product", ()=> {
        rmap.addProduct(vertex)
      })
    }
    if (!rmap.isSelected(vertex)) {
      addMenuItem(menu, "Select", ()=> {
        rmap.addSelection(vertex)
      })
    }
    addMenuItem(menu, "Jmol", ()=> {
      vertex.openJmol()
    })
    menu
  }

  def addMenuItem(menu : javax.swing.JPopupMenu, label : String, action : ()=>Unit) : javax.swing.JMenuItem = {
    val item: javax.swing.JMenuItem = new javax.swing.JMenuItem(label)
    item.addActionListener(new java.awt.event.ActionListener() {
      def actionPerformed(e : java.awt.event.ActionEvent) = {
        action()
        menu.setVisible(false)
      }
    })
    menu.add(item)
  }

  var positions:Map[Vertex, (Double, Double)] = Map()

  def update(aspect:GraphAspect) = {
    if (aspect == GraphAspect.SELECTION || aspect == GraphAspect.MAP) {
      val minEnergy = if (rmap.vertices.isEmpty) 0.0 else rmap.vertices.map(_.energy).min
      val maxEnergy = if (rmap.vertices.isEmpty) 1.0 else rmap.vertices.map(_.energy).max
      def y(v:Vertex) = (maxEnergy - v.energy) / (maxEnergy-minEnergy)
      val newPositions:mutable.Map[Vertex,(Double,Double)] = mutable.Map()
      val done:mutable.Set[Vertex] = mutable.Set()
      var maxHop:Int = 0
      def stuff(vertices:Set[Vertex]):Unit = {
        done ++= vertices
        for (v <- vertices)
          newPositions.update(v, (maxHop * last_label_width, y(v)))
        maxHop += 1
        val newHop:Set[Vertex] =
          vertices.map(
            (v)=>v.edges.toSet[Edge].map((e)=>e.peer(v).getOrElse(v)).filter((v2:Vertex)=> !done.contains(v2))).reduce(_++_)
        if (!newHop.isEmpty) {
          maxHop += 1
          stuff(newHop)
        }
      }
      if (!rmap.reactants.isEmpty)
        stuff(rmap.reactants.toSet)
      positions = newPositions.toMap
      preferredSize = new Dimension(((maxHop + 1) * last_label_width).toInt, 350)
    }
    repaint()
  }

  override def paint(graphics : Graphics2D) {
    graphics.setStroke(new BasicStroke(1))
    graphics.setColor(Color.WHITE)
    val width: Int = size.width
    val height: Int = size.height
    val fontMetrics = graphics.getFontMetrics
    val labelMargin = 2
    val LABEL_WIDTH: Int =
      if (rmap.vertices.isEmpty)
        0
      else
        rmap.vertices.map((v)=>fontMetrics.stringWidth(v.label)).reduce(Math.max) + labelMargin * 2
    val LABEL_HEIGHT: Int = fontMetrics.getHeight + labelMargin * 2
    last_label_width = LABEL_WIDTH
    last_label_height = LABEL_HEIGHT
    def Y(y:Double) = (y * (height - LABEL_HEIGHT*2)+LABEL_HEIGHT).toInt
    val lineColor = Color.LIGHT_GRAY
    graphics.setColor(lineColor)
    for (edge <- rmap.edges) {
      positions.get(edge.vertex1) match {
        case Some((x1:Double, y1:Double)) => positions.get(edge.vertex2) match {
          case Some((x2:Double, y2:Double)) =>
            if (x1 < x2) graphics.drawLine(x1.toInt+LABEL_WIDTH, Y(y1), x2.toInt, Y(y2))
            if (x2 < x1) graphics.drawLine(x2.toInt+LABEL_WIDTH, Y(y2), x1.toInt, Y(y1))
          case None => ()
        }
        case None => ()
      }
    }
    for (vertex <- rmap.vertices) {
      positions.get(vertex) match {
        case Some((x0, y0)) =>
          val x: Int = x0.toInt
          val y: Int = Y(y0)-LABEL_HEIGHT
          val labelOffsetY = LABEL_HEIGHT - labelMargin - fontMetrics.getDescent
          val labelOffsetX = (LABEL_WIDTH-fontMetrics.stringWidth(vertex.label)) / 2
          graphics.setStroke(new BasicStroke(3))
          graphics.setColor(lineColor)
          graphics.drawLine(x, y+LABEL_HEIGHT, x+LABEL_WIDTH, y+LABEL_HEIGHT)
          graphics.setStroke(new BasicStroke(1))
          if (rmap.isReactant(vertex)) {
            graphics.setColor(lineColor)
            graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
            graphics.setColor(Color.BLUE)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
          } else if (rmap.isProduct(vertex)) {
            graphics.setColor(lineColor)
            graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
            graphics.setColor(Color.BLUE)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
            graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
            graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
          } else {
            graphics.setColor(lineColor)
            graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
          }
        case None => ()
      }
    }
    graphics.setColor(Color.BLACK)
    rmap.path match {
      case None => ()
      case Some(path) =>
        var vertices = path.vertices
        val stroke = graphics.getStroke
        for (index <- 0 until vertices.size - 1) {
          val v1 = vertices(index)
          val v2 = vertices(index+1)
          positions.get(v1) match {
            case Some((x1:Double, y1:Double)) => positions.get(v2) match {
              case Some((x2:Double, y2:Double)) =>
                if (x1 < x2) graphics.drawLine(x1.toInt+LABEL_WIDTH, Y(y1), x2.toInt, Y(y2))
                if (x2 < x1) graphics.drawLine(x2.toInt+LABEL_WIDTH, Y(y2), x1.toInt, Y(y1))
              case None => ()
            }
            case None => ()
          }
        }
        for (vertex <- vertices) {
          positions.get(vertex) match {
            case Some((x0:Double, y0:Double)) =>
              val x: Int = x0.toInt
              val y: Int = Y(y0)-LABEL_HEIGHT
              val labelOffsetY = LABEL_HEIGHT - labelMargin - fontMetrics.getDescent
              val labelOffsetX = (LABEL_WIDTH-fontMetrics.stringWidth(vertex.label)) / 2
              graphics.setStroke(new BasicStroke(3))
              graphics.setColor(Color.BLACK)
              graphics.drawLine(x, y+LABEL_HEIGHT, x+LABEL_WIDTH, y+LABEL_HEIGHT)
              graphics.setStroke(new BasicStroke(1))
              if (rmap.isReactant(vertex)) {
                graphics.setColor(Color.BLACK)
                graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
                graphics.setColor(Color.BLUE)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
              } else if (rmap.isProduct(vertex)) {
                graphics.setColor(Color.BLACK)
                graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
                graphics.setColor(Color.BLUE)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
                graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
                graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
              } else {
                graphics.setColor(Color.BLACK)
                graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
              }
            case None => ()
          }
        }
    }
  }

  def hoverVertex(x: Int, y: Int): Option[Vertex] = {
    val width: Int = size.width
    val height: Int = size.height
    def Y(y:Double) = (y * (height - last_label_height*2)+last_label_height).toInt
    var hover: Option[Vertex] = None
    for (vertex <- rmap.vertices) {
      positions.get(vertex) match {
        case Some((vx:Double, vy:Double)) =>
          if (vx  <= x
            && x <= vx + last_label_width
            && Y(vy) - last_label_height <= y
            && y <= Y(vy))
            hover = Some(vertex)
        case None => ()
      }
    }
    hover
  }
}
