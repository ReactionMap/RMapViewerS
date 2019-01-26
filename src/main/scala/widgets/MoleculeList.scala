package widgets

import java.awt.Color
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

import application.{RMapViewer, ScratchingBrowser}
import graph.{ReactionMap, Vertex}

import scala.swing._
import scala.swing.event.{ListSelectionChanged, MouseClicked}

/**
  * Created by tomohiro on 2016/02/24.
  */
class MoleculeList extends ScrollPane {
  def rmap:ReactionMap = RMapViewer.rmap

  val listView: ListView[ImageIcon] = new ListView[ImageIcon]() {
    selection.intervalMode = ListView.IntervalMode.MultiInterval
    listenTo(mouse.clicks)
    listenTo(selection)
/*
    reactions += {
      case ListSelectionChanged(source, range, live) =>
        rmap.setSelections(source.selection.indices.toList.map(listVertices(_)))
    }
    listenTo(mouse.clicks)
    reactions += {
      case e: MouseClicked =>
        if (e.peer.getButton > 1 && listView.selection.indices.nonEmpty) {
          RMapViewer.mapPanel.popupMenu(listVertices(listView.selection.indices.head)).show(ScratchingBrowser.moleculeList.peer, e.point.x, e.point.y)
        }
    }
    listenTo(selection)
*/
  }

  val selection = listView.selection
  def width:Int = size.width

  var listVertices: List[Vertex] = List()


  def clear():Unit = {
    setList(List())
  }

  def selectedVertices():List[Vertex] = {
    listView.selection.indices.toList.sorted.map(listVertices(_))
  }

  def setList(vertices: List[Vertex]):Unit = {
    listVertices = vertices
    listView.listData = vertices.map((vertex: Vertex) => {
      val image = new BufferedImage(width - 30, 64, BufferedImage.TYPE_INT_BGR)
      val graphics = image.createGraphics()
      graphics.setPaint(Color.white)
      graphics.fillRect(0, 0, image.getWidth, image.getHeight)
      val thumbnail = vertex.image
      graphics.drawImage(thumbnail, image.getWidth-64, 0, 64, 64, 0, 0, thumbnail.getWidth(), thumbnail.getHeight(), ScratchingBrowser.peer)
      val fontMetrics = graphics.getFontMetrics()
      graphics.setPaint(Color.BLACK)
      graphics.drawString(vertex.label, 0, (64 - fontMetrics.getAscent) / 2)
      new ImageIcon(image)
    })
    self repaint()
  }

  def update(vertices: List[Vertex], selection:List[Vertex]):Unit = {
    listVertices = vertices
    listView.listData = vertices.map((vertex: Vertex) => {
      val image = new BufferedImage(width - 30, 64, BufferedImage.TYPE_INT_BGR)
      val graphics = image.createGraphics()
      graphics.setPaint(Color.white)
      graphics.fillRect(0, 0, image.getWidth, image.getHeight)
      val thumbnail = vertex.image
      graphics.drawImage(thumbnail, image.getWidth-64, 0, 64, 64, 0, 0, thumbnail.getWidth(), thumbnail.getHeight(), ScratchingBrowser.peer)
      val fontMetrics = graphics.getFontMetrics()
      graphics.setPaint(Color.BLACK)
      graphics.drawString(vertex.label, 0, (64 - fontMetrics.getAscent) / 2)
      new ImageIcon(image)
    })
    listView.selection.indices.clear()
    selection.foreach(vertices.indexOf(_) match {
      case -1 => ()
      case i:Int => listView.selection.indices += i
    })
    self repaint()
  }
  viewportView = listView
  preferredSize = new Dimension(200, 500)
}
