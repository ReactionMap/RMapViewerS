package application

import graph.Vertex
import widgets.{MoleculeList, MoleculeSketch, PeriodicTable}

import scala.swing._
import scala.swing.event.ButtonClicked

/**
  * Created by tomohiro on 2016/02/23.
  */
object ScratchingBrowser extends Frame {
  val periodicTable = new PeriodicTable()
  val moleculeSketch = new MoleculeSketch()
  val moleculeList = new MoleculeList()
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
      contents += new Button("Search") {
        reactions += {
          case _: ButtonClicked =>
            updateMoleculeList()
        }
      }
      contents += moleculeList
    }
  }

  def updateMoleculeList() = {
    val geometrySignature = moleculeSketch.geometrySignature
    val structureSignature = moleculeSketch.structureSignature
    moleculeList.update(RMapViewer.rmap.vertices.toList
      .filter((vertex:Vertex)=>vertex.structureSignature == structureSignature)
      .map((vertex: Vertex) => (vertex, geometrySignature.dist(vertex.geometrySignature)))
      .sortWith { case (less: (Vertex, Double), more: (Vertex, Double)) => less._2 <= more._2 }
      .map(_._1))
  }

  def atom = periodicTable.value
}

