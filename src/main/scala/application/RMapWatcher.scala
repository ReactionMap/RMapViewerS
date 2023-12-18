package application

import importer.{GRRMDigestImporter, GRRMFullImporter, TrajectoryImporter}

import java.awt.Color
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.io.{File, FileNotFoundException}
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}
import scala.swing.{Action, BoxPanel, Button, Color, ComboBox, Component, Dialog, Frame, GridPanel, Label, Orientation, Panel, Table, TextField, event}
import scala.swing.Dialog.{Message, Options, Result}

object RMapWatcher extends Frame {
  override def closeOperation(): Unit = stopWatching()
  var watchItems: List[WatchItem] = List()
  val watchList:WTable = new WTable()
  class WTable extends Table {
    title = "RMap Watcher"
    override lazy val model: javax.swing.table.DefaultTableModel = super.model.asInstanceOf[javax.swing.table.DefaultTableModel]
    def getRowCount():Int = model.getRowCount()
    def addRow():Unit = model.addRow(Array():Array[Object])
    def removeRow():Unit = model.removeRow(model.getRowCount()-1)
    selection.intervalMode = Table.IntervalMode.Single
    selection.elementMode = Table.ElementMode.Row
    selectionBackground = new Color(200,200,255)
    showGrid=false
    model.addColumn("")
    model.addColumn("name")
    model.addColumn("last fetch")
    model.addColumn("next fetch")
    model.addColumn("EQs")
    model.addColumn("TSs")
    addRow()
    addRow()
    addRow()
    reactions += {
      case event.TableColumnsSelected(source, range, false) => updateButtons()
    }
  }
  def selectionIndex():Option[Int] = {
    val rows = watchList.selection.rows
    if (rows.isEmpty)
      None
    else
      Some(rows.leadIndex)
  }

  val addButton = new Button(new Action("Add") {
    override def apply(): Unit = {
      val dialog: WatchDialog = new WatchDialog()
      Dialog.showConfirmation(message=dialog.contents.head.peer, title="Watch", optionType=Options.OkCancel) match {
        case Result.Ok => {
          watchItems = watchItems ++ List(dialog.watchItem())
          updateTable()
        }
      }
    }
  })

  def removeButton() = new Button(new Action("Remove") {
    override def apply(): Unit = {
      selectionIndex() match {
        case Some(index) => {
          watchItems = watchItems.take(index) ++ watchItems.drop(index + 1)
          updateTable()
        }
      }
    }
  })

  def editButton() = new Button(new Action("Edit") {
    override def apply(): Unit = {}
  })

  def fetchButton() = new Button(new Action("Fetch") {
    override def apply(): Unit = {
      selectionIndex() match {
        case Some(index) => {
          watchItems(index).fetch()
          updateTable()
        }
        case None => {}
      }
    }
  })

  def openButton() = new Button(new Action("Open") {
    override def apply(): Unit = {
      selectionIndex() match {
        case Some(index) => {
          watchItems(index).rmapJson match {
            case Some(rmap) => RMapViewer.rmap.readFrom(rmap)
            case None => {}
          }
        }
        case None => {}
      }
    }
  })

  contents = new BoxPanel(Orientation.Vertical) {
    contents += watchList
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += addButton
      contents += removeButton
      contents += editButton
      contents += fetchButton
      contents += openButton
    }
  }

  def updateButtons(): Unit = {
    selectionIndex match {
      case Some(index) => {
        removeButton.enabled = true
        editButton.enabled = true
        fetchButton.enabled = true
        openButton.enabled = true
      }
      case None => {
        removeButton.enabled = false
        editButton.enabled = false
        fetchButton.enabled = false
        openButton.enabled = false
      }
    }
  }

  def updateTable(): Unit = {
    while (watchList.model.getRowCount < watchItems.length)
      watchList.addRow()
    while (watchList.model.getRowCount() > watchItems.length)
      watchList.removeRow()
    for {r <- 0 to watchItems.length - 1} {
      var item:WatchItem = watchItems(r)
      watchList.model.setValueAt(item.status(), r, 0)
      watchList.model.setValueAt(item.name, r, 1)
      watchList.model.setValueAt(item.lastFetchString(), r, 2)
      watchList.model.setValueAt(item.nextFetchString(), r, 3)
      watchList.model.setValueAt(item.EQs(), r, 4)
      watchList.model.setValueAt(item.TSs(), r, 5)
    }
    updateButtons()
  }
  val scheduler:ScheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1)
  def startWatching(): Unit = {
    scheduler.scheduleAtFixedRate(new Runnable() {override def run():Unit = watch()}, 1, 1, TimeUnit.SECONDS)
  }
  def stopWatching(): Unit = {
    scheduler shutdownNow()
  }
  def watch(): Unit = {
    var changed: Boolean = false
    for (item:WatchItem <- watchItems) {
      if (item.watch()) changed = true
    }
    if (changed) updateTable()
  }
  updateButtons()
  startWatching()
}

class WatchDialog() extends Frame {

  val nameField: TextField = new TextField("")
  val typeDropList: ComboBox[DirectoryType] = new ComboBox[DirectoryType](List[DirectoryType](Trajectory, GRRMDigest, GRRMFull))
  val directoryField: TextField = new TextField("") {
    editable = false
  }
  val chooseDirectoryButton: Button = new Button(new Action("...") {
    override def apply(): Unit = {
      import java.awt.FileDialog
      val file_dialog: FileDialog = new FileDialog(null: javax.swing.JFrame, "Select a .com file", FileDialog.LOAD)
      file_dialog.setVisible(true)
      val filename: String = file_dialog.getFile
      val dirname: String = file_dialog.getDirectory
      if (filename != null && dirname != null)
        directoryField.text = dirname + "/" + filename
    }
  })
  val intervalField: TextField = new TextField("3.0")
   contents = new GridPanel(4, 2) {
    contents += new Label("Name")
    contents += nameField
    contents += new Label("Type")
    contents += typeDropList
    contents += new Label("Directory")
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += directoryField
      contents += chooseDirectoryButton
    }
    contents += new Label("Interval")
    contents += intervalField
  }

  def watchItem(): WatchItem = new WatchItem(nameField.text, directoryField.text, typeDropList.selection.item, intervalField.text.toDouble)
}

case class WatchItem(name: String, file: String, directoryType: DirectoryType, interval: Double) {
  var lastFetch: Option[LocalDateTime] = None
  var nextFetch: Option[LocalDateTime] = Some(LocalDateTime.now().plusSeconds((interval*3600).toLong))
  var paused:Boolean = false
  var rmapJson : Option[Map[String, Any]] = None
  def pause():Unit = paused = !paused
  def watch():Boolean = {
    nextFetch match {
      case Some(next) => if (next.isBefore(LocalDateTime.now())) fetch() else false
      case None => false
    }
  }
  def fetch():Boolean = {
    try {
      lastFetch = Some(LocalDateTime.now())
      nextFetch = Some(lastFetch.get.plusSeconds((interval*3600).toLong))
      rmapJson = Some(directoryType.retrieve(this))
      true
    } catch {
      case e: Exception => false
    }
  }
  def lastFetchString():String = {
    lastFetch match {
      case Some(time) => time.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
      case None => " - "
    }
  }
  def nextFetchString():String = {
    nextFetch match {
      case Some(time) => time.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
      case None => " - "
    }
  }
  def status(): String = {
    if (paused)
      "P"
    else if (rmapJson.isEmpty && LocalDateTime.now().isAfter(nextFetch.get))
      "E"
    else
      ""
  }
  def EQs(): String = {
    rmapJson match {
      case Some(rmap) =>
        (rmap.get("vertices").get.asInstanceOf[List[Map[String, Any]]].count(v => v.get("label").get.asInstanceOf[String](0) == 'E')).toString
      case None => " - "
    }
  }
  def TSs(): String = {
    rmapJson match {
      case Some(rmap) =>
        (rmap.get("vertices").get.asInstanceOf[List[Map[String, Any]]].count(v => v.get("label").get.asInstanceOf[String](0) == 'T')).toString
      case None => " - "
    }
  }
}

abstract class DirectoryType {
  def retrieve(item: WatchItem): Map[String, Any]
}
case object GRRMFull extends DirectoryType {
  def retrieve(item: WatchItem): Map[String, Any] = new GRRMFullImporter(new File(item.file)).toJSON
  override def toString():String = "GRRM (full)"
}
case object GRRMDigest extends DirectoryType {
  def retrieve(item: WatchItem): Map[String, Any] = new GRRMDigestImporter(new File(item.file)).toJSON
  override def toString():String = "GRRM (digest)"
}
case object Trajectory extends DirectoryType {
  def retrieve(item: WatchItem): Map[String, Any] = new TrajectoryImporter(new File(item.file)).toJSON
  override def toString():String = "Trajectory"
}
