package application

import javax.swing.ImageIcon

import graph.Vertex
import org.json4s.jackson.JsonMethods
import skinny.http._

import scala.swing._
import scala.swing.event.{ButtonClicked, ListSelectionChanged}

object RMapClient extends Frame {
    override val title = "RMapClient"
  var rmaps:List[Map[String,Any]] = List()
  val serverURLField = new TextField("http://localhost:1701/", 15)
  val nameField = new TextField(15)
  val command = new TextField()
  val method = new TextField()
  val basis = new TextField()
  val components = new Table(
    Array(
      Array("C", ""),
      Array("H", ""),
      Array("O", ""),
      Array("", ""),
      Array("", ""),
      Array("", "")):Array[Array[Any]],
    List("element", "quantity"))
  val nameLabel = new Label("") {xLayoutAlignment = 0.0}
  val numEQ = new Label("")
  val numTS = new Label("")
  val numDC = new Label("")
  val stableEQs = new BoxPanel(Orientation.Horizontal) {
    minimumSize = new Dimension(300, 70)
  }

  val rmapList = new ListView[String](List("")) {
    fixedCellWidth=130
    listenTo(selection)
    reactions += {
      case _ => updateRMapView()
    }
  }
  val openButton = new Button("Open") {
    reactions += {
      case _:ButtonClicked => {
        openRMap()
        RMapClient.close()
      }
    }
    enabled = false
  }

  val grrmText = new TextArea()
  contents = new BoxPanel(Orientation.Horizontal) {
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("URL")
        contents += serverURLField
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("rmap name")
        contents += nameField
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("grrm")
        contents += command
        contents += new Label("method")
        contents += method
        contents += new Label("basis")
        contents += basis
      }
      contents += components
      contents += new Button("Search") {
        reactions += {
          case _: ButtonClicked =>
            updateRMapList()
        }

      }
    }
    contents += new ScrollPane(rmapList) {
      minimumSize = new Dimension(150,150)
    }
    contents += new BoxPanel(Orientation.Vertical) {
      contents += nameLabel
      nameLabel.xLayoutAlignment = 0.0
      contents += new BoxPanel(Orientation.Horizontal) {
        xLayoutAlignment = 0.0
        contents += numEQ
        contents += new Label(" EQs, ")
        contents += numTS
        contents += new Label(" TSs, ")
        contents += numDC
        contents += new Label(" DCs")
      }
      contents += new ScrollPane(stableEQs) {
        xLayoutAlignment = 0.0
        minimumSize = new Dimension(300, 150)
      }
      contents += openButton
      openButton.xLayoutAlignment = 0.0

    }
  }

  def updateRMapList():Unit = {
    val elemsAndCounts:List[(String, String)] =
      Range(0, components.model.getRowCount).toList.map(
        (r:Int) => components.model.getValueAt(r, 0).asInstanceOf[String].trim.capitalize
          -> components.model.getValueAt(r, 1).asInstanceOf[String]).
        filter((pair) => pair match {case (atom:String , count:String) => !(atom.isEmpty || count.isEmpty) })
    val request = elemsAndCounts.foldLeft(Request(serverURLField.text+"api/search").
      queryParam("name" -> nameField.text).
      queryParam("command1" -> command.text).
      queryParam("command2" -> method.text).
      queryParam("command3" -> basis.text))((req, pair) => req.queryParam("atoms."+pair._1, pair._2))
    val response:Response = HTTP.get(request)
    rmaps = JsonMethods.parse(response.textBody).values.asInstanceOf[List[Map[String, Any]]]
    rmapList.listData = rmaps.map((rmap)=>rmap("meta").asInstanceOf[Map[String, Any]]("name").asInstanceOf[String])
    rmapList.selectIndices(-1)
    openButton.enabled = false
  }
  def updateRMapView():Unit = {
    val indices = rmapList.selection.indices
    if (indices.nonEmpty) {
      val index = indices.head
      val rmap:Map[String, Any] = rmaps(index)
      val meta:Map[String, Any] = rmap("meta").asInstanceOf[Map[String, Any]]
      nameLabel.text = meta("name").asInstanceOf[String]
      numEQ.text = meta("numEQ").toString
      numTS.text = meta("numTS").toString
      numDC.text = meta("numDC").toString
      stableEQs.contents.clear()
      val vertices = meta("stableEQs").asInstanceOf[List[Map[String, Any]]].map((json)=>Vertex(json))
      if (vertices.nonEmpty) {
        val e0 = vertices.head.energy
        vertices.foreach((vertex)=> {
          val v = vertex
          vertex.baselineEnergy = e0
          stableEQs.contents += new BoxPanel(Orientation.Vertical) {
            contents += new Label(vertex.label)
            contents += new Label{icon = new ImageIcon(vertex.createImage(100))}
            contents += new Label(vertex.kJmolString)
            listenTo(mouse.clicks)
            reactions += {
              case _:event.MouseClicked => v.openJmol()
            }
          }
        }
        )
      }
      openButton.enabled = true
    } else {
      nameLabel.text = ""
      numEQ.text = "-"
      numTS.text = "-"
      numDC.text = "-"
      stableEQs.contents.clear()
      openButton.enabled = false
    }
  }
  def openRMap():Unit = {
    if (rmapList.selection.indices.isEmpty) return
    val index = rmapList.selection.indices.head
    val jsonRMap = rmaps(index)
    val id = rmaps(rmapList.selection.indices.head)("_id")
    val request = Request(serverURLField.text+"api/get").queryParam("_id" -> id.toString)
    val response:Response = HTTP.get(request)
    val rmap = JsonMethods.parse(response.textBody).values.asInstanceOf[Map[String, Any]]
    RMapViewer.rmap.readFrom(rmap)
  }
}
