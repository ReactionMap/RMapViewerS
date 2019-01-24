package widgets

import java.awt.{BasicStroke, Color}

import geometry.Point
import graph.searcher._
import graph.{ReactionPath, GraphAspect, ReactionMap, Vertex}
import numerizer._

import scala.math._
import scala.swing._
import scala.swing.event._

/**
 * Created by tomohiro on 2014/07/31.
 */
case class MapPanel(rmap : ReactionMap) extends Panel {
  background = Color.WHITE
  rmap.dependents += ((aspect:GraphAspect)=>update(aspect))
  preferredSize = new Dimension(400, 400)
  private var offset_x: Double = 0.5
  private var offset_y: Double = 0.5
  private var scale: Double = 0.9
  private var last_mouse_x : Int = 0
  private var last_mouse_y : Int = 0
  private var dragging : Boolean = false
  private var last_label_width:Int = 0
  private var last_label_height:Int = 0

  listenTo(mouse.clicks, mouse.moves, mouse.wheel)
  reactions += {
    case event: MouseDragged =>
      if (dragging) {
        drag(event.point.x - last_mouse_x, event.point.y - last_mouse_y)
      }
      dragging = true
      last_mouse_x = event.point.x
      last_mouse_y = event.point.y
    case event: MouseReleased => dragging = false
    case event: MouseWheelMoved => zoom(event.rotation)
    case event: MouseClicked =>
      if (event.peer.getButton > 1)
        hoverVertex match {
          case None => ()
          case Some(vertex) => popupMenu(vertex).show(peer, event.point.x, event.point.y)
        }
      else {
        hoverVertex = hoverVertex(event.point.x, event.point.y)
        repaint()
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
    menu.addSeparator()
    addMenuItem(menu, "Info", ()=> {
      vertex.showInfo()
    })
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

  def update(aspect:GraphAspect) = {
    if (aspect == GraphAspect.MAP) {
      xNumerizer = xNumerizer.renew()
      yNumerizer = yNumerizer.renew()
    }
    if (aspect == GraphAspect.SELECTION) {
      search()
    }
    repaint()
  }

  val searchers:List[ShortestPathSearcher] = List(
    new EnergyGapShortestPathSearcher(rmap, true),
    new EnergyGapShortestPathSearcher(rmap, false),
    new EnergyShortestPathSearcher(rmap, true),
    new EnergyShortestPathSearcher(rmap, false),
    new SimpleShortestPathSearcher(rmap, true),
    new SimpleShortestPathSearcher(rmap, false)
  )
  def searcherComboBox() = {
    new ComboBox(searchers.map(_.name)) {
      selection.reactions += {
        case SelectionChanged(_) =>
          searcher = searchers(selection.index)
        case _ => ()
      }
    }
  }

  private var _searcher = searchers(0)
  def searcher = _searcher
  def searcher_=(s:ShortestPathSearcher) = {
    _searcher = s
    search()
  }

  def search() = {
    rmap.paths = _searcher.search().map(new ReactionPath(_))
  }

  var renderingModes = List("Label", "Geometry")
  def renderingMode = renderingModes(rendererComboBox.selection.index)

  override def paint(graphics : Graphics2D) = {
    if (renderingMode == "Geometry")
      paintImage(graphics)
    else
      paintLabel(graphics)
  }


  def paintLabel(graphics : Graphics2D) {
    xNumerizer.update()
    yNumerizer.update()
    graphics.setPaint(Color.WHITE)
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
    val lineColor = if (rmap.path == None && hoverVertex == None) Color.BLACK else Color.LIGHT_GRAY
    val labelColor = if (rmap.path == None && hoverVertex == None) Color.LIGHT_GRAY else Color.LIGHT_GRAY.brighter()
    graphics.setPaint(lineColor)
    for (edge <- rmap.edges) {
      positionToPoint(xNumerizer(edge.vertex1), yNumerizer(edge.vertex1), width, height) match {
        case Some(point1) => positionToPoint(xNumerizer(edge.vertex2), yNumerizer(edge.vertex2), width, height) match {
          case Some(point2) => graphics.drawLine(
            point1.x.toInt,
            point1.y.toInt,
            point2.x.toInt,
            point2.y.toInt)
          case None => ()
        }
        case None => ()
      }
    }
    for (vertex <- rmap.vertices) {
      positionToPoint(xNumerizer(vertex), yNumerizer(vertex), width, height) match {
        case Some(point) =>
          val x: Int = point.x.toInt-LABEL_WIDTH/2
          val y: Int = point.y.toInt-LABEL_HEIGHT/2
          val labelOffsetY = LABEL_HEIGHT - labelMargin - fontMetrics.getDescent
          val labelOffsetX = (LABEL_WIDTH-fontMetrics.stringWidth(vertex.label)) / 2
          if (vertex.label.charAt(0) == 'E') graphics.setPaint(labelColor) else graphics.setPaint(Color.WHITE)
          graphics.fillRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
          if (rmap.isReactant(vertex)) {
            graphics.setPaint(lineColor)
            graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
            graphics.setPaint(Color.BLUE)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
          } else if (rmap.isProduct(vertex)) {
            graphics.setPaint(lineColor)
            graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
            graphics.setPaint(Color.BLUE)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
            graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
            graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
          } else if (rmap.isSelected(vertex)) {
            graphics.setPaint(Color.BLACK)
            graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
            graphics.setPaint(Color.RED)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
            graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
            graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
          } else {
            graphics.setPaint(lineColor)
            graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
          }
        case None => ()
      }
    }
    graphics.setPaint(Color.BLACK)
    rmap.path match {
      case None => ()
      case Some(path) =>
        var vertices = path.vertices
        val stroke = graphics.getStroke
        graphics.setStroke(new BasicStroke(2))
        for (index <- 0 until vertices.size - 1) {
          val v1 = vertices(index)
          val v2 = vertices(index+1)
          this.positionToPoint(xNumerizer(v1), yNumerizer(v1), width, height) match {
            case Some(point1) => this.positionToPoint(xNumerizer(v2), yNumerizer(v2), width, height) match {
              case Some(point2) =>
                graphics.drawLine(
                  point1.x.toInt,
                  point1.y.toInt,
                  point2.x.toInt,
                  point2.y.toInt)
              case None => ()
            }
            case None => ()
          }
        }
        graphics.setStroke(stroke)
        for (vertex <- vertices) {
          this.positionToPoint(xNumerizer(vertex), yNumerizer(vertex), width, height) match {
            case Some(point) =>
              val x: Int = point.x.toInt-LABEL_WIDTH/2
              val y: Int = point.y.toInt-LABEL_HEIGHT/2
              val labelOffsetY = LABEL_HEIGHT - labelMargin - fontMetrics.getDescent
              val labelOffsetX = (LABEL_WIDTH-fontMetrics.stringWidth(vertex.label)) / 2
              if (vertex.label.charAt(0) == 'E') graphics.setPaint(Color.LIGHT_GRAY) else graphics.setPaint(Color.WHITE)
              graphics.fillRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
              if (rmap.isReactant(vertex)) {
                graphics.setPaint(Color.BLACK)
                graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
                graphics.setPaint(Color.BLUE)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
              } else if (rmap.isProduct(vertex)) {
                graphics.setPaint(Color.BLACK)
                graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
                graphics.setPaint(Color.BLUE)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
                graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
                graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
              } else if (rmap.isSelected(vertex)) {
                graphics.setPaint(Color.BLACK)
                graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
                graphics.setPaint(Color.RED)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
                graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
                graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
              } else {
                graphics.setPaint(Color.BLACK)
                graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
              }
            case None => ()
          }
        }
    }
    hoverVertex match {
      case Some(vertex) =>
        positionToPoint(xNumerizer(vertex), yNumerizer(vertex), width, height) match {
          case Some(point) =>
            val x: Int = point.x.toInt-LABEL_WIDTH/2
            val y: Int = point.y.toInt-LABEL_HEIGHT/2
            val labelOffsetY = LABEL_HEIGHT - labelMargin - fontMetrics.getDescent
            val labelOffsetX = (LABEL_WIDTH-fontMetrics.stringWidth(vertex.label)) / 2
            if (vertex.label.charAt(0) == 'E') graphics.setPaint(labelColor) else graphics.setPaint(Color.WHITE)
            graphics.fillRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
              graphics.setPaint(Color.BLACK)
              graphics.drawString(vertex.label, x + labelOffsetX, y + labelOffsetY)
              graphics.setPaint(Color.BLACK)
              graphics.setStroke(new BasicStroke(3))
              graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
          case None => ()
        }
      case None => ()
    }
  }

  def paintImage(graphics : Graphics2D) {
    xNumerizer.update()
    yNumerizer.update()
    graphics.setPaint(Color.WHITE)
    val width: Int = size.width
    val height: Int = size.height
    val fontMetrics = graphics.getFontMetrics
    val labelMargin = 2
    val LABEL_WIDTH: Int = 64
    val LABEL_HEIGHT: Int = 64
    last_label_width = LABEL_WIDTH
    last_label_height = LABEL_HEIGHT
    val lineColor = if (rmap.path == None) Color.BLACK else Color.LIGHT_GRAY
    val labelColor = if (rmap.path == None) Color.LIGHT_GRAY else Color.LIGHT_GRAY.brighter()
    graphics.setPaint(lineColor)
    for (edge <- rmap.edges) {
      positionToPoint(xNumerizer(edge.vertex1), yNumerizer(edge.vertex1), width, height) match {
        case Some(point1) => positionToPoint(xNumerizer(edge.vertex2), yNumerizer(edge.vertex2), width, height) match {
          case Some(point2) => graphics.drawLine(
            point1.x.toInt,
            point1.y.toInt,
            point2.x.toInt,
            point2.y.toInt)
          case None => ()
        }
        case None => ()
      }
    }
    for (vertex <- rmap.vertices) {
      positionToPoint(xNumerizer(vertex), yNumerizer(vertex), width, height) match {
        case Some(point) =>
          val x: Int = point.x.toInt-LABEL_WIDTH/2
          val y: Int = point.y.toInt-LABEL_HEIGHT/2
          val labelOffsetY = LABEL_HEIGHT - labelMargin - fontMetrics.getDescent
          val labelOffsetX = (LABEL_WIDTH-fontMetrics.stringWidth(vertex.label)) / 2
          graphics.setPaint(Color.BLACK)
          graphics.drawImage(vertex.image, x, y, null)
          graphics.drawString(vertex.label, x, y+LABEL_HEIGHT)
          if (rmap.isReactant(vertex)) {
            graphics.setPaint(Color.BLUE)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
          } else if (rmap.isProduct(vertex)) {
            graphics.setPaint(Color.BLUE)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
            graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
            graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
          } else if (rmap.isSelected(vertex)) {
            graphics.setPaint(Color.RED)
            graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
            graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
            graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
            graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
          } else {
          }
        case None => ()
      }
    }
    graphics.setPaint(Color.BLACK)
    rmap.path match {
      case None => ()
      case Some(path) =>
        var vertices = path.vertices
        val stroke = graphics.getStroke
        graphics.setStroke(new BasicStroke(2))
        for (index <- 0 until vertices.size - 1) {
          val v1 = vertices(index)
          val v2 = vertices(index+1)
          this.positionToPoint(xNumerizer(v1), yNumerizer(v1), width, height) match {
            case Some(point1) => this.positionToPoint(xNumerizer(v2), yNumerizer(v2), width, height) match {
              case Some(point2) =>
                graphics.drawLine(
                  point1.x.toInt,
                  point1.y.toInt,
                  point2.x.toInt,
                  point2.y.toInt)
              case None => ()
            }
            case None => ()
          }
        }
        graphics.setStroke(stroke)
        for (vertex <- vertices) {
          this.positionToPoint(xNumerizer(vertex), yNumerizer(vertex), width, height) match {
            case Some(point) =>
              val x: Int = point.x.toInt-LABEL_WIDTH/2
              val y: Int = point.y.toInt-LABEL_HEIGHT/2
              val labelOffsetY = LABEL_HEIGHT - labelMargin - fontMetrics.getDescent
              val labelOffsetX = (LABEL_WIDTH-fontMetrics.stringWidth(vertex.label)) / 2
              graphics.setPaint(Color.BLACK)
              graphics.drawImage(vertex.image, x, y, null)
              graphics.drawImage(vertex.image, x, y, null)
              if (rmap.isReactant(vertex)) {
                graphics.setPaint(Color.BLUE)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
              } else if (rmap.isProduct(vertex)) {
                graphics.setPaint(Color.BLUE)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
                graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
                graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
              } else if (rmap.isSelected(vertex)) {
                graphics.setPaint(Color.RED)
                graphics.drawRect(x, y, LABEL_WIDTH, LABEL_HEIGHT)
                graphics.drawRect(x-1, y-1, LABEL_WIDTH+2, LABEL_HEIGHT+2)
                graphics.drawRect(x-3, y-3, LABEL_WIDTH+6, LABEL_HEIGHT+6)
                graphics.drawRect(x-4, y-4, LABEL_WIDTH+8, LABEL_HEIGHT+8)
              } else {
              }
            case None => ()
          }
        }
    }
  }
  def positionToPoint(x:Option[Double], y:Option[Double], width: Int, height: Int): Option[Point] =
    x match {
      case Some(x1) => y match {
        case Some(y1) => Some(Point(
          ((x1 - offset_x) * width * scale) + width / 2,
          ((1.0 - y1 - offset_y) * height * scale) + height / 2))
        case None => None
      }
      case None => None
    }


  def pointToPosition(point: Point): Point = pointToPosition(point, size.width, size.height)

  def pointToPosition(point: Point, width: Int, height: Int): Point =
    Point((point.x - width / 2) / (width * scale) + offset_x,
          (point.y - height / 2) / (height * scale) + offset_x)

  def drag(x: Int, y: Int) {
    offset_x -= x.toDouble / size.width / scale
    offset_y -= y.toDouble / size.height / scale
    repaint()
  }

  def zoom(zoom: Int) {
    scale *= pow(1.1, zoom)
    repaint()
  }
  def hoverVertex(x: Int, y: Int): Option[Vertex] = {
    val width: Int = size.width
    val height: Int = size.height
    var hover: Option[Vertex] = None
    for (vertex <- rmap.vertices) {
      positionToPoint(xNumerizer(vertex), yNumerizer(vertex), width, height) match {
        case Some(point) =>
          if (point.x - last_label_width / 2 <= x
            && x <= point.x + last_label_width / 2
            && point.y - last_label_height / 2 <= y
            && y <= point.y + last_label_height / 2)
            hover = Some(vertex)
        case None => ()
      }
    }
    hover
  }
  var hoverVertex:Option[Vertex] = None

  private var _xNumerizer:Numerizer = new MapXNumerizer(rmap)
  private var _yNumerizer:Numerizer = new MapYNumerizer(rmap)
  def xNumerizer = _xNumerizer
  def yNumerizer = _yNumerizer
  def xNumerizer_=(numerizer:Numerizer) = {
    _xNumerizer = _xNumerizer.transitionTo(numerizer.renew(), ((_)=>repaint()), ((to)=>_xNumerizer = to))
    repaint()
  }
  def yNumerizer_=(numerizer:Numerizer) = {
    _yNumerizer = _yNumerizer.transitionTo(numerizer.renew(), ((_)=>repaint()), ((to)=>_yNumerizer = to))
    repaint()
  }
  val xNumerizers:List[Numerizer] = List(
    new MapXNumerizer(rmap),
    new KindNumerizer(rmap),
    new HopsNumerizer(rmap, ((_)=>repaint()), ((numerizer)=>_xNumerizer=numerizer)),
    new SimilarityXNumerizer(rmap, ((_)=>repaint()), ((numerizer)=>_xNumerizer=numerizer))
  )
  val yNumerizers:List[Numerizer] = List(
    new MapYNumerizer(rmap),
    new EnergyNumerizer(rmap),
    new MaxEnergyGapNumerizer(rmap, ((_)=>repaint()), ((numerizer)=>_yNumerizer=numerizer)),
    new SimilarityYNumerizer(rmap, ((_)=>repaint()), ((numerizer)=>_yNumerizer=numerizer))
  )
  def xNumerizerComboBox() = {
    new ComboBox(xNumerizers.map(_.name)) {
      selection.reactions += {
        case SelectionChanged(_) =>
          xNumerizer = xNumerizers(selection.index)
        case _ => ()
      }
    }
  }
  def yNumerizerComboBox() = {
    new ComboBox(yNumerizers.map(_.name)) {
      selection.reactions += {
        case SelectionChanged(_) =>
          yNumerizer = yNumerizers(selection.index)
      }
    }
  }
  val rendererComboBox = {
    new ComboBox(renderingModes) {
      selection.reactions += {
        case SelectionChanged(_) =>
          rmap.changed(GraphAspect.RENDERING)
      }
    }
  }
}
