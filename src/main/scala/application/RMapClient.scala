package application

import java.io.FileReader

import graph.Vertex
import org.json4s.ReaderInput
import org.json4s.jackson.JsonMethods
import skinny.http._

import scala.swing._
import scala.swing.event.ButtonClicked

object RMapClient extends Frame {
    override val title = "RMapClient"
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
  val rmapList = new ListView[String](List(""))
  rmapList.fixedCellWidth=200
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
      contents += new ScrollPane(rmapList) {preferredSize = new Dimension(300,100)}
    }

    def updateRMapList() = {
      val elemsAndCounts:List[(String, String)] =
        Range(0, components.model.getRowCount()).toList.map(
          (r:Int) => components.model.getValueAt(r, 0).asInstanceOf[String].trim.capitalize
            -> components.model.getValueAt(r, 1).asInstanceOf[String]).
          filter((pair) => pair match {case (atom:String , count:String) => !(atom.isEmpty || count.isEmpty) })
      val request = elemsAndCounts.foldLeft(Request(serverURLField.text+"api/search").
        queryParam("name" -> nameField.text).
        queryParam("command1" -> command.text).
        queryParam("command2" -> method.text).
        queryParam("command3" -> basis.text))((req, pair) => req.queryParam(pair._1, pair._2))
      val response:Response = HTTP.get(request)
      val rmaps:List[Map[String, Any]] = JsonMethods.parse(response.textBody).values.asInstanceOf[List[Map[String, Any]]]
      rmapList.listData = rmaps.map((rmap)=>rmap("meta").asInstanceOf[Map[String, Any]]("name").asInstanceOf[String])

    }
}
