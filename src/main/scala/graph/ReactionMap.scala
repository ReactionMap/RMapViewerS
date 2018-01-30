package graph

/**
  * Created by tomohiro on 2014/07/31.
  */

import java.io._

import application.{RMapClient, ScratchingBrowser}
import org.json4s._
import org.json4s.jackson.{JsonMethods, Serialization}

import scala.collection.mutable
import scala.swing.Dialog

class ReactionMap() {
  var grrm: String = ""
  var vertices: List[Vertex] = List()
  var edges: List[Edge] = List()
  val reactants: mutable.Set[Vertex] = mutable.Set()
  val products: mutable.Set[Vertex] = mutable.Set()
  val selections: mutable.Set[Vertex] = mutable.Set()
  private var _paths: List[ReactionPath] = List[ReactionPath]()
  private var _path: Option[ReactionPath] = None
  val dependents: mutable.MutableList[GraphAspect => Unit] = mutable.MutableList()

  def changed(aspect: GraphAspect) = dependents.foreach((f) => f(aspect))

  var maxNumberOfHops: Int = 7

  private var _progress: Double = 0.0

  def progress = _progress

  def progress_=(v: Double) = {
    _progress = v
    changed(GraphAspect.PROGRESS)
  }

  def paths: List[ReactionPath] = _paths

  def paths_=(newPaths: List[ReactionPath]): Unit = {
    _paths = newPaths
    changed(GraphAspect.PATHS)
  }

  def path: Option[ReactionPath] = _path

  def path_=(newPath: Option[ReactionPath]): Unit = {
    _path = newPath
    changed(GraphAspect.PATH)
  }

  def setVerticesAndEdges(vs: List[Vertex], es: List[Edge]):Unit = {
    val e0 = vs.map((v)=>v.energy).min
    vs.foreach((v)=>v.baselineEnergy = e0)
    vertices = vs
    edges = es
    reactants.clear()
    products.clear()
    selections.clear()
    paths = List()
    changed(GraphAspect.MAP)
  }

  def addReactant(vertex: Vertex):Unit = {
    reactants += vertex
    products -= vertex
    selections -= vertex
    changed(GraphAspect.SELECTION)
  }

  def isReactant(vertex: Vertex): Boolean = {
    reactants contains vertex
  }

  def addProduct(vertex: Vertex):Unit = {
    reactants -= vertex
    products += vertex
    selections -= vertex
    changed(GraphAspect.SELECTION)
  }

  def isProduct(vertex: Vertex): Boolean = {
    products contains vertex
  }

  def setSelections(vertices: List[Vertex]):Unit = {
    selections.clear()
    selections ++= vertices
    changed(GraphAspect.SELECTION)
  }

  def addSelection(vertex: Vertex):Unit = {
    reactants -= vertex
    products -= vertex
    selections += vertex
    changed(GraphAspect.SELECTION)
  }

  def isSelected(vertex: Vertex): Boolean = {
    selections contains vertex
  }

  def addSelectionsToReactants():Unit = {
    reactants ++= selections
    setSelections(List())
  }

  def addSelectionsToProducts():Unit = {
    products ++= selections
    setSelections(List())
  }

  def openScratchingBrowser():Unit = {
    ScratchingBrowser.open()
  }

  def unselect(vertex: Vertex):Unit = {
    reactants -= vertex
    products -= vertex
    selections -= vertex
    changed(GraphAspect.SELECTION)
  }

  def unselectAll():Unit = {
    setSelections(List())
  }

  def searchLabel():Unit = {
    Dialog.showInput[String](null, "Search Label: ", "RMapViewer", initial = "") match {
      case Some(query) => setSelections(vertices.filter(_.label == query))
      case  None => ()
    }
  }

  def searchSmiles():Unit = {
    Dialog.showInput[String](null, "Search SMILES: ", "RMapViewer", initial = "") match {
      case Some(query) => setSelections(vertices.filter(_.smiles.contains(query)))
      case  None => ()
    }
  }

  def searchInchi():Unit = {
    Dialog.showInput[String](null, "Search InChI: ", "RMapViewer", initial = "") match {
      case Some(query) => setSelections(vertices.filter(_.inchi.contains(query)))
      case  None => ()
    }
  }

  def searchCanost():Unit = {
    Dialog.showInput[String](null, "Search CAST-1D: ", "RMapViewer", initial = "") match {
      case Some(query) => setSelections(vertices.filter(_.canost.contains(query)))
      case  None => ()
    }
  }

  def isEffective(vertex: Vertex): Boolean = {
    true
  }

  def readFrom(filename: File): Unit = {
    readFrom(JsonMethods.parse(ReaderInput(new FileReader(filename))).values.asInstanceOf[Map[String, Any]])
  }

  def openRmapClient():Unit = {
    RMapClient.open()
  }

  def openRmap(vs:List[Vertex], es:List[Edge], grrm:String):Unit = {
    this.grrm = grrm
    setVerticesAndEdges(vs, es)
  }

  def readFrom(json: Map[String, Any]): Unit = {
    val vertices = json("vertices").asInstanceOf[List[Map[String, Any]]].map(json_vertex => Vertex(json_vertex))
    val edges = json("edges").asInstanceOf[List[Map[String, BigInt]]].map(json_edge =>
      Edge(
        vertices(json_edge("vertex1").intValue()),
        vertices(json_edge("vertex2").intValue()),
        json_edge))
    grrm = json.get("grrm") match {
      case Some(grrmopts) => grrmopts.asInstanceOf[String]
      case None => ""
    }
    println("MAP:" + vertices(0).label + " bonds: " + vertices(0).bonds)
    setVerticesAndEdges(vertices, edges)
  }

  def openRmapFile():Unit = {
    import java.awt.FileDialog
    val file_dialog: FileDialog = new FileDialog(null: javax.swing.JFrame, "Choose an rmap file", FileDialog.LOAD)
    file_dialog.setVisible(true)
    val filename: String = file_dialog.getFile
    val dirname: String = file_dialog.getDirectory
    if (filename != null && dirname != null)
      try {
        readFrom(new File(dirname + "/" + filename))
      } catch {
        case e: FileNotFoundException => Dialog.showMessage(title = "RMapViewer", message = "Can not read " + dirname + "/" + filename)
        case e: Exception => Dialog.showMessage(title = "RMapViewer", message = "Error when reading " + dirname + "/" + filename)
      }
  }

  def writeTo(filename: File): Unit = {
    implicit val formats = org.json4s.DefaultFormats
    try {
      val writer: PrintWriter = new PrintWriter(filename)
      writer.write(Serialization.write(Map(
        "vertices" -> vertices.map(_.json),
        "edges" -> edges.map(_.json),
        "grrm" -> grrm)))
      writer.close()
    } catch {
      case e: FileNotFoundException => Dialog.showMessage(title = "RMapViewer", message = "Can not write to " + filename.getName())
      case e: Exception => Dialog.showMessage(title = "RMapViewer", message = "Error when writing to " + filename.getName())
    }
  }

  def saveRmapFile():Unit = {
    import java.awt.FileDialog
    val file_dialog: FileDialog = new FileDialog(null: javax.swing.JFrame, "Saving an rmap file", FileDialog.SAVE)
    file_dialog.setVisible(true)
    val filename: String = file_dialog.getFile
    val dirname: String = file_dialog.getDirectory
    if (filename != null && dirname != null)
      writeTo(new File(dirname + "/" + filename))
  }

  def importFrom(dirname: File, comfile: String): Unit = {
    readFrom(new GRRMImporter(dirname, comfile).toJSON)
  }

  def importFromGRRM():Unit = {
    import java.awt.FileDialog
    val file_dialog: FileDialog = new FileDialog(null: javax.swing.JFrame, "Select a .com file", FileDialog.LOAD)
    file_dialog.setVisible(true)
    val filename: String = file_dialog.getFile
    val dirname: String = file_dialog.getDirectory
    if (filename != null && dirname != null)
      if (filename.endsWith(".com")) {
        try {
          importFrom(new File(dirname), filename)
        } catch {
          case e: FileNotFoundException => Dialog.showMessage(title = "RMapViewer", message = e.getMessage())
          case e: Exception => Dialog.showMessage(title = "RMapViewer", message = "Error while importing:\n"+e.getMessage())
        }
      } else {
        Dialog.showMessage(title = "RMapViewer", message = "Please select a .com file.")
      }
  }
}
