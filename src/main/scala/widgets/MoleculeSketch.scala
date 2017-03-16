package widgets


import java.awt.{Dimension, BasicStroke, Color, Font}

import atom.Atom
import geometry.{StructureSignature, GeometrySignature, Point}
import application.ScratchingBrowser

import scala.swing.event.{MouseClicked, MouseWheelMoved, MouseReleased, MouseDragged}
import scala.swing.{Graphics2D, Panel}

/**
  * Created by tomohiro on 2016/02/23.
  */
class MoleculeSketch extends Panel {
  def atomInds:Map[Int,Int] = {
    val ids:List[Int] = atoms.keys.toList.sorted
    ids.indices.map((i:Int)=>ids(i)->i).toMap
  }
  def geometry:List[List[Any]] =
    atoms.keys.toList.sorted.map(atoms(_)).map{case (atom:Atom, point:Point)=>List(atom.symbol, point.x, point.y, point.z)}
  def geometrySignature:GeometrySignature = new GeometrySignature(geometry)
  def structureSignature:StructureSignature = {
    val inds = atomInds
    println("structure signature = "+new StructureSignature(geometry, bonds.map((pair:((Int,Int),Int))=>(inds(pair._1._1), inds(pair._1._2), pair._2)).toList).value)
    new StructureSignature(geometry, bonds.map((pair:((Int,Int),Int))=>(inds(pair._1._1), inds(pair._1._2), pair._2)).toList)
  }

  var _scale = 40.0
  def scale = _scale
  def scale_= (newScale:Double) = {
    _scale = newScale
    repaint()
  }
  val precision = 0.5
  var id: Int = 0
  private var _atoms: Map[Int, (Atom, Point)] = Map()
  def atoms = _atoms
  def atoms_= (newAtoms:Map[Int, (Atom, Point)]) = {
    _atoms = newAtoms
    repaint()
  }
  private var _bonds: Map[(Int, Int), Int] = Map()
  def bonds = _bonds
  def bonds_= (newBonds:Map[(Int,Int),Int]) = {
    _bonds = newBonds
    repaint()
  }

  def getAtomIdAt(point:Point):Option[Int] = atoms.keys.toList.find(
      (id1: Int) => atoms(id1) match {
        case (_: Atom, point1: Point) => point.dist(point1) <= precision
      })

  def getBindIndexAt(point:Point):Option[(Int,Int)] = bonds.keys.toList.find{
    case (atomId1:Int, atomId2:Int) => point.dist((atoms(atomId1)._2+atoms(atomId2)._2)/2.0)<= precision}

  def addAtom(point:Point): Int = {
    val theId = id
    id += 1
    atoms += theId ->(ScratchingBrowser.atom, point)
    theId
  }

  def removeAtom(atomId:Int) = {
    removeBond(atomId)
    atoms -= atomId
  }

  def changeAtom(atomId:Int) = {
    val position = atoms(atomId)._2
    atoms = atoms.updated(atomId, (ScratchingBrowser.atom, position))
  }

  def addBond(fromId: Int, toPoint:Point) = {
    val pair0 = (fromId, atoms.keys.toList.find(
      (id1: Int) => atoms(id1) match {
        case (_: Atom, point1: Point) => toPoint.dist(point1) <= precision
      }
    ) match {
      case Some(toId: Int) => toId
      case None => addAtom(toPoint)
    })
    val pair = (Math.min(pair0._1, pair0._2), Math.max(pair0._1, pair0._2))
    bonds.get(pair) match {
      case Some(bind) => bonds = bonds.updated(pair, bind % 3 + 1)
      case None => bonds += pair -> 1
    }
  }

  def addBond(atomId1:Int, atomId2:Int) = {
    bonds = bonds.updated((atomId1, atomId2), bonds((atomId1, atomId2)) % 3 + 1)
  }

  def removeBond(atomId:Int) =
    bonds = bonds.filterKeys{
      case (fromId:Int, toId:Int)=> fromId != atomId && toId != atomId }

  def removeBond(atomId1:Int, atomId2:Int) = {
    val fromId:Int = Math.min(atomId1, atomId2)
    val toId:Int = Math.max(atomId1, atomId2)
    bonds -= ((fromId, toId))
  }

  def clear() = {
    bonds = Map()
    atoms = Map()
  }

  override def paint(graphics:Graphics2D) {
    val font:Font = new Font("Arial", Font.BOLD, (scale*0.5).toInt)
    graphics.setFont(font)
    val fontMetrics = graphics.getFontMetrics
    val offsetX = size.width / 2
    val offsetY = size.height / 2
    for (((from, to), bind) <- bonds) {
      val (fromAtom:Atom, fromPoint:Point) = atoms(from)
      val (toAtom:Atom, toPoint:Point) = atoms(to)
      val crossVector:Point = new Point(fromPoint.y - toPoint.y, toPoint.x - fromPoint.x).unit
      val centerBind = (bind-1).toDouble / 2.0
      graphics.setColor(Color.BLACK)
      graphics.setStroke(new BasicStroke(2))
      for (bindIndex <- 0 until bind) {
        val crossOffset = crossVector * ((bindIndex - centerBind) * 0.1)
        val fromP = fromPoint + crossOffset
        val toP = toPoint + crossOffset
        graphics.drawLine(
          Math.round(fromP.x * scale + offsetX).toInt,
          Math.round(fromP.y * scale * -1 + offsetY).toInt,
          Math.round(toP.x * scale + offsetX).toInt,
          Math.round(toP.y * scale * -1 + offsetY).toInt)
      }
    }
    for ((atom:Atom, point:Point) <- atoms.values) {
      val symbol:String = atom.symbol
      val radius:Int = (atom.vanDerWaalsRadius * scale / 4.0).toInt
      val x = (point.x * scale + offsetX).toInt
      val y = (point.y * scale * -1 + offsetY).toInt
      graphics.setPaint(atom.color)
      graphics.fillOval(x-radius, y-radius, radius*2, radius*2)
      graphics.setPaint(Color.BLACK)
      graphics.drawString(
        symbol,
        x - fontMetrics.stringWidth(symbol)/2,
        y + (fontMetrics.getAscent/2).toInt)
    }
  }

  var dragStartAtom:Option[Int] = None
  listenTo(mouse.clicks, mouse.moves, mouse.wheel)
  reactions += {
    case event: MouseClicked =>
        dragStartAtom = None
        var point = new Point((event.point.x - size.width / 2) / scale, (size.height / 2 - event.point.y) / scale)
        getAtomIdAt(point) match {
          case Some(atomId) =>
            if (event.modifiers == 0) {
              val oldSymbol = atoms(atomId)._1.symbol
              val newSymbol = ScratchingBrowser.atom.symbol
              if (oldSymbol != newSymbol) {
                changeAtom(atomId)
              }
            } else {
              atomMenu(atomId).show(peer, event.point.x, event.point.y)
            }
          case None =>
            getBindIndexAt(point) match {
              case Some(pair) =>
                if (event.modifiers == 0)
                  addBond(pair._1, pair._2)
                else
                  bondMenu(pair._1, pair._2).show(peer, event.point.x, event.point.y)
              case None =>
                addAtom(point)
            }
        }
    case event: MouseDragged =>
      dragStartAtom match {
        case Some(_) =>
        case None =>
          val point = new Point((event.point.x - size.width/2)/scale, (size.height/2 - event.point.y)/scale)
          dragStartAtom = Some(getAtomIdAt(point) match {
            case Some(atomId) => atomId
            case None => addAtom(point)
          })
      }
    case event: MouseReleased =>
      dragStartAtom match {
        case Some(fromId) =>
          val point = new Point((event.point.x - size.width/2)/scale, (size.height/2 - event.point.y)/scale)
          getAtomIdAt(point) match {
            case Some(_) => addBond(fromId, point)
            case None =>
              val fromPoint = atoms(fromId)._2
              val theta = Math.round((point-fromPoint).theta / Math.PI * 6.0) * Math.PI / 6.0
              val d = atoms(fromId)._1.covalentRadius + ScratchingBrowser.atom.covalentRadius
              val toPoint = new Point(Math.cos(theta)*d+fromPoint.x, Math.sin(theta)*d+fromPoint.y)
              addBond(fromId, toPoint)

          }
          dragStartAtom = None
        case none =>
      }
    case event: MouseWheelMoved =>
      scale = Math.max(20.0, Math.min(80.0, scale * Math.pow(1.1, event.rotation)))
      repaint()
  }

  def atomMenu(atomId:Int):javax.swing.JPopupMenu = {
    val menu: javax.swing.JPopupMenu = new javax.swing.JPopupMenu
    addMenuItem(menu, "Delete", ()=> {
      removeAtom(atomId)
    })
    addMenuItem(menu, "Change", ()=> {
      changeAtom(atomId)
    })
    menu
  }

  def bondMenu(atomId1:Int, atomId2:Int):javax.swing.JPopupMenu = {
    val menu: javax.swing.JPopupMenu = new javax.swing.JPopupMenu
    addMenuItem(menu, "Delete", ()=> {
      removeBond(atomId1, atomId2)
    })
    addMenuItem(menu, "Change", ()=> {
      addBond(atomId1, atomId2)
    })
    menu
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


  preferredSize = new Dimension(400, 300)

}
