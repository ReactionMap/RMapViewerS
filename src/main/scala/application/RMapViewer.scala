package application

import graph.{GraphAspect, ReactionMap, Vertex}
import widgets._

import scala.swing._
import scala.swing.event.MouseClicked

/**
  * Created by tomohiro on 2014/07/31.
  */
object RMapViewer extends SimpleSwingApplication {
  val rmap = new ReactionMap()

  def top = new MainFrame {
    title = "RMapViewer"
    menuBar = new MenuBar() {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Open rmap file...") {
            def apply:Unit = {
              rmap.openRmapFile()
            }
          }
        )
        contents += new MenuItem(new Action("Open GRRM directory...") {
            def apply:Unit = {
              rmap.importFromGRRMFull()
            }
          }
        )
        contents += new MenuItem(new Action("Open digest GRRM directory...") {
          def apply:Unit = {
            rmap.importFromGRRMDigest()
          }
        }
        )
        contents += new MenuItem(new Action("Open trajectory directory...") {
          def apply:Unit = {
            rmap.importFromTrajectory()
          }
        }
        )
        contents += new Separator()
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
        contents += new MenuItem(new Action("Open watcher...") {
          def apply: Unit = {
            RMapWatcher.open()
          }
        })
      }
      contents += new Menu("Edit") {
        contents += new MenuItem("Unselect all") {
          action = new Action("Unselect all") {
            def apply: Unit = {
              rmap.unselectAll()
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

  val selectionList = new MoleculeList() {
    rmap.dependents += ((aspect:GraphAspect)=>update(aspect))
    listenTo(listView.mouse.clicks)
    reactions += {
      case e: MouseClicked =>
        if (e.peer.getButton > 1)
          popupMenu.show(peer, e.point.x, e.point.y)
    }

    def popupMenu():javax.swing.JPopupMenu = {
      val menu: javax.swing.JPopupMenu = new javax.swing.JPopupMenu
      addMenuItem(menu, "select all", ()=>listView.selection.indices ++= (0 to listVertices.size - 1))
      addMenuItem(menu, "unselect all", ()=>listView.selection.indices.clear)
      addMenuItem(menu, "move to reactants", ()=> rmap.moveToReactant(selectedVertices))
      addMenuItem(menu, "move to products", ()=> rmap.moveToProduct(selectedVertices))
      addMenuItem(menu, "remove", ()=>rmap.unselectAll(selectedVertices))
      menu
    }
    def update(aspect:GraphAspect) = {
      aspect match {
        case GraphAspect.SELECTION => {
          setList(rmap.selections.toList.sortBy(_.energy))
        }
        case _ => ()
      }
    }
  }

  val reactantList = new MoleculeList() {
    rmap.dependents += ((aspect:GraphAspect)=>update(aspect))
    listenTo(listView.mouse.clicks)
    reactions += {
      case e: MouseClicked =>
        if (e.peer.getButton > 1)
          popupMenu.show(peer, e.point.x, e.point.y)
    }

    def popupMenu():javax.swing.JPopupMenu = {
      val menu: javax.swing.JPopupMenu = new javax.swing.JPopupMenu
      addMenuItem(menu, "select all", ()=>listView.selection.indices ++= (0 to listVertices.size - 1))
      addMenuItem(menu, "unselect all", ()=>listView.selection.indices.clear)
      addMenuItem(menu, "move to selections", ()=> rmap.moveToSelection(selectedVertices))
      addMenuItem(menu, "remove", ()=>rmap.unselectAll(selectedVertices))
      menu
    }
    def update(aspect:GraphAspect) = {
      aspect match {
        case GraphAspect.SELECTION => {
          setList(rmap.reactants.toList.sortBy(_.energy))
        }
        case _ => ()
      }
    }
  }

  val productList = new MoleculeList() {
    rmap.dependents += ((aspect:GraphAspect)=>update(aspect))
    listenTo(listView.mouse.clicks)
    reactions += {
      case e: MouseClicked =>
        if (e.peer.getButton > 1)
          popupMenu.show(peer, e.point.x, e.point.y)
    }

    def popupMenu():javax.swing.JPopupMenu = {
      val menu: javax.swing.JPopupMenu = new javax.swing.JPopupMenu
      addMenuItem(menu, "select all", ()=>listView.selection.indices ++= (0 to listVertices.size - 1))
      addMenuItem(menu, "unselect all", ()=>listView.selection.indices.clear)
      addMenuItem(menu, "move to selections", ()=> rmap.moveToSelection(selectedVertices))
      addMenuItem(menu, "remove", ()=>rmap.unselectAll(selectedVertices))
      menu
    }
    def update(aspect:GraphAspect) = {
      aspect match {
        case GraphAspect.SELECTION => {
          setList(rmap.products.toList.sortBy(_.energy))
        }
        case _ => ()
      }
    }
  }

  val mapPanel: MapPanel = MapPanel(rmap)
  val mapPane = new BoxPanel(Orientation.Horizontal) {
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new Label("Selections")
      contents += selectionList
    }
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Vertical) {
        contents += new Label("Reactants")
        contents += reactantList
      }
      contents += new BoxPanel(Orientation.Vertical) {
        contents += new Label("Products")
        contents += productList
      }
    }
    contents += new BorderPanel {
      add(new BoxPanel(Orientation.Horizontal) {
        contents += mapPanel
        contents += new ScrollPane(EnergyTreePanel(rmap)) {
          preferredSize = new Dimension(400, 400)
        }
      },
        BorderPanel.Position.Center)
      add(new BoxPanel(Orientation.Horizontal) {
        contents += mapPanel.xNumerizerComboBox()
        contents += mapPanel.yNumerizerComboBox()
        contents += mapPanel.rendererComboBox
        contents += mapPanel.searcherComboBox()
      },
        BorderPanel.Position.South)
    }
  }

  val pathList = new BorderPanel {
    add(new ScrollPane(PathList(rmap)), BorderPanel.Position.Center)
    add(ProgressBar(rmap), BorderPanel.Position.South)
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

}
