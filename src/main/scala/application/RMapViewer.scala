package application

import graph.ReactionMap
import widgets._

import scala.swing._

/**
  * Created by tomohiro on 2014/07/31.
  */
object RMapViewer extends SimpleSwingApplication {
  val rmap = new ReactionMap()

  def top = new MainFrame {
    title = "RMapViewer"
    menuBar = new MenuBar() {
      contents += new Menu("File") {
        contents += new MenuItem("Open...") {
          action = new Action("Open...") {
            def apply:Unit = {
              rmap.openRmapFile()
            }
          }
        }
        contents += new MenuItem("Save...") {
          action = new Action("Save...") {
            def apply:Unit = {
              rmap.saveRmapFile()
            }
          }
        }
        contents += new Separator()
        contents += new MenuItem("Open server...") {
          action = new Action("Open server...") {
            def apply:Unit = {
              rmap.openRmapClient()
            }
          }
        }
        contents += new Separator()
        contents += new MenuItem("Import...") {
          action = new Action("Import...") {
            def apply:Unit = {
              rmap.importFromGRRM()
            }
          }
        }
      }
      contents += new Menu("Edit") {
        contents += new MenuItem("Unselect all") {
          action = new Action("Unselect all") {
            def apply:Unit = {
              rmap.unselectAll()
            }
          }
        }
        contents += new Separator()
        contents += new MenuItem("Add selection to reactants") {
          action = new Action("Add selection to reactants") {
            def apply:Unit = {
              rmap.addSelectionsToReactants()
            }
          }
        }
        contents += new MenuItem("Add selection to products") {
          action = new Action("Add selection to products") {
            def apply:Unit = {
              rmap.addSelectionsToProducts()
            }
          }
        }
      }

      contents += new Menu("Search") {
        contents += new MenuItem("Label...") {
          action = new Action("Label...") {
            def apply:Unit = {
              rmap.searchLabel()
            }
          }
        }
        contents += new MenuItem("SMILES...") {
          action = new Action("SMILES...") {
            def apply:Unit = {
              rmap.searchSmiles()
            }
          }
        }
        contents += new MenuItem("InChI...") {
          action = new Action("InChI...") {
            def apply:Unit = {
              rmap.searchInchi()
            }
          }
        }
        contents += new MenuItem("CAST-1D...") {
          action = new Action("CAST-1D...") {
            def apply:Unit = {
              rmap.searchCanost()
            }
          }
        }
        contents += new MenuItem("Draw...") {
          action = new Action("Draw...") {
            def apply:Unit = {
              rmap.openScratchingBrowser()
            }
          }
        }
      }
    }
    contents = new BorderPanel {
      add(mapPane, BorderPanel.Position.Center)
      add(pathList, BorderPanel.Position.South)
    }
  }

  val mapPanel: MapPanel = MapPanel(rmap)
  val mapPane = new BorderPanel {
    add(new BoxPanel(Orientation.Horizontal) {
      contents += mapPanel
      contents += new ScrollPane(EnergyTreePanel(rmap)) {
        preferredSize = new Dimension(400, 400)
      }
    }, BorderPanel.Position.Center)
    add(new BoxPanel(Orientation.Horizontal) {
      contents += mapPanel.xNumerizerComboBox()
      contents += mapPanel.yNumerizerComboBox()
      contents += mapPanel.rendererComboBox
      contents += mapPanel.searcherComboBox()
    }, BorderPanel.Position.South)
  }

  val pathList = new BorderPanel {
    add(new ScrollPane(PathList(rmap)), BorderPanel.Position.Center)
    add(ProgressBar(rmap), BorderPanel.Position.South)
  }
}
