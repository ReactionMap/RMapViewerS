package widgets

import java.awt.Color
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

import application.{RMapViewer, ScratchingBrowser}
import graph.Vertex
import scala.swing._
import scala.swing.event.{MouseClicked, ListSelectionChanged}

/**
  * Created by tomohiro on 2016/02/24.
  */
class MoleculeList extends ScrollPane {
  def rmap = RMapViewer.rmap

  val listView: ListView[ImageIcon] = new ListView[ImageIcon]() {
    selection.intervalMode = ListView.IntervalMode.MultiInterval

    object Command extends Publisher {
      reactions += {
        case ListSelectionChanged(source, range, live) =>
         rmap.setSelections(source.selection.indices.toList.map(listVertices(_)))
      }
      listenTo(mouse.clicks)
      reactions += {
        case e: MouseClicked =>
          if (e.peer.getButton() > 1 && listView.selection.indices.nonEmpty) {
            RMapViewer.mapPanel.popupMenu(listVertices(listView.selection.indices.head)).show(ScratchingBrowser.moleculeList.peer, e.point.x, e.point.y)
          }
      }
    }

    Command.listenTo(selection)
  }

  def width = size.width

  var listVertices: List[Vertex] = List()

  def update(vertices: List[Vertex]) = {
    listVertices = vertices
    listView.listData = vertices.map((vertex: Vertex) => {
      val image = new BufferedImage(width - 30, 64, BufferedImage.TYPE_INT_BGR)
      val graphics = image.createGraphics()
      graphics.setPaint(Color.white)
      graphics.fillRect(0, 0, image.getWidth, image.getHeight)
      val thumbnail = vertex.image
      graphics.drawImage(thumbnail, 0, 0, 64, 64, 0, 0, thumbnail.getWidth(), thumbnail.getHeight(), ScratchingBrowser.peer)
      val fontMetrics = graphics.getFontMetrics()
      graphics.setPaint(Color.BLACK)
      graphics.drawString(vertex.label, 70, (64 - fontMetrics.getAscent) / 2)
      new ImageIcon(image)
    })
    self repaint()
  }

  viewportView = listView
  preferredSize = new Dimension(200, 500)
}
