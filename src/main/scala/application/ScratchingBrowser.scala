package application

import graph.Vertex
import widgets.{MoleculeList, MoleculeSketch, PeriodicTable}

import scala.swing._
import scala.swing.event.{ButtonClicked, ListSelectionChanged, MouseClicked}

/**
  * Created by tomohiro on 2016/02/23.
  */
object ScratchingBrowser extends Frame {
  val periodicTable = new PeriodicTable()
  val moleculeSketch = new MoleculeSketch()
  val sortByEnergyCheckBox = new CheckBox("Sort by energy") {
    selected = true
    reactions += {
      case _: ButtonClicked => updateMoleculeList()
    }
  }
  val moleculeList = new MoleculeList() {
    listenTo(selection)
    reactions += {
      case ListSelectionChanged(source, range, live) =>
        rmap.setSelections(source.selection.indices.toList.map(listVertices(_)))
    }
    listenTo(listView.mouse.clicks)
    reactions += {
      case e: MouseClicked =>
        if (e.peer.getButton > 1 && listView.selection.indices.nonEmpty)
          RMapViewer.mapPanel.popupMenu(listVertices(listView.selection.indices.head)).show(peer, e.point.x, e.point.y)
    }
  }

  override val title = "RMapViewer"
  contents = new BoxPanel(Orientation.Horizontal) {
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new Button("Clear all") {
        reactions += {
          case _: ButtonClicked =>
            moleculeSketch.clear()
        }
      }
      contents += moleculeSketch
      contents += periodicTable
    }
    contents += new BoxPanel(Orientation.Vertical) {

      contents += sortByEnergyCheckBox
      contents += new Button("Search") {
        reactions += {
          case _: ButtonClicked =>
            updateMoleculeList()
        }
      }
      contents += moleculeList
    }
  }

  def updateMoleculeList():Unit = {
    val command = moleculeSketch.generateMatchingCommands(RMapViewer.rmap.vertices.head.geometry.map(_.head.asInstanceOf[String]))
    if (sortByEnergyCheckBox.selected) {
      moleculeList.update(RMapViewer.rmap.vertices.toList
        .filter((vertex:Vertex)=>moleculeSketch.matches(command, vertex.geometry, vertex.bonds))
        .sortWith { case (less: Vertex, more: Vertex) => less.energy <= more.energy })
    } else {
      val geometrySignature = moleculeSketch.geometrySignature
      moleculeList.update(RMapViewer.rmap.vertices.toList
        .filter((vertex:Vertex)=>moleculeSketch.matches(command, vertex.geometry, vertex.bonds))
        .map((vertex: Vertex) => (vertex, geometrySignature.dist(vertex.geometrySignature)))
        .sortWith { case (less: (Vertex, Double), more: (Vertex, Double)) => less._2 <= more._2 }
        .map(_._1))
    }
  }

  def atom = periodicTable.value
}

