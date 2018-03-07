package widgets


import java.awt.{Dimension, BasicStroke, Color, Font}

import atom.Atom
import geometry.{StructureSignature, GeometrySignature, Point}
import utilities.Bag
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
    new StructureSignature(geometry, bonds.map((pair:((Int,Int),Int))=>(inds(pair._1._1), inds(pair._1._2), pair._2)).toList)
  }

  var _scale:Double = 40.0
  def scale:Double = _scale
  def scale_= (newScale:Double):Unit = {
    _scale = newScale
    repaint()
  }
  val precision = 0.5
  var id: Int = 0
  private var _atoms: Map[Int, (Atom, Point)] = Map()
  def atoms:Map[Int, (Atom, Point)] = _atoms
  def atoms_= (newAtoms:Map[Int, (Atom, Point)]):Unit = {
    _atoms = newAtoms
    repaint()
  }
  private var _bonds: Map[(Int, Int), Int] = Map()
  def bonds:Map[(Int, Int), Int] = _bonds
  def bonds_= (newBonds:Map[(Int,Int),Int]):Unit = {
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

  def removeAtom(atomId:Int):Unit = {
    removeBond(atomId)
    atoms -= atomId
  }

  def changeAtom(atomId:Int):Unit = {
    val position = atoms(atomId)._2
    atoms = atoms.updated(atomId, (ScratchingBrowser.atom, position))
  }

  def addBond(fromId: Int, toPoint:Point):Unit = {
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

  def addBond(atomId1:Int, atomId2:Int):Unit = {
    bonds = bonds.updated((atomId1, atomId2), bonds((atomId1, atomId2)) % 3 + 1)
  }

  def removeBond(atomId:Int):Unit =
    bonds = bonds.filterKeys{
      case (fromId:Int, toId:Int)=> fromId != atomId && toId != atomId }

  def removeBond(atomId1:Int, atomId2:Int):Unit = {
    val fromId:Int = Math.min(atomId1, atomId2)
    val toId:Int = Math.max(atomId1, atomId2)
    bonds -= ((fromId, toId))
  }

  def clear():Unit = {
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
      val crossVector:Point = Point(fromPoint.y - toPoint.y, toPoint.x - fromPoint.x).unit
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
        var point = Point((event.point.x - size.width / 2) / scale, (size.height / 2 - event.point.y) / scale)
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
          val point = Point((event.point.x - size.width/2)/scale, (size.height/2 - event.point.y)/scale)
          dragStartAtom = Some(getAtomIdAt(point) match {
            case Some(atomId) => atomId
            case None => addAtom(point)
          })
      }
    case event: MouseReleased =>
      dragStartAtom match {
        case Some(fromId) =>
          val point = Point((event.point.x - size.width/2)/scale, (size.height/2 - event.point.y)/scale)
          getAtomIdAt(point) match {
            case Some(_) => addBond(fromId, point)
            case None =>
              val fromPoint = atoms(fromId)._2
              val theta = Math.round((point-fromPoint).theta / Math.PI * 6.0) * Math.PI / 6.0
              val d = atoms(fromId)._1.covalentRadius + ScratchingBrowser.atom.covalentRadius
              val toPoint = Point(Math.cos(theta)*d+fromPoint.x, Math.sin(theta)*d+fromPoint.y)
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
      def actionPerformed(e : java.awt.event.ActionEvent):Unit = {
        action()
        menu.setVisible(false)
      }
    })
    menu.add(item)
  }

  def matches(commands:List[(Either[String, Int], Either[String, Int])], atoms2 : List[List[Any]], bonds2 : List[(Int, Int, Int)]) : Boolean = {
    val superElementBag: Bag[String] =
      new Bag[String](atoms2.map((a) => a.head.asInstanceOf[String]))
    val subElementBag: Bag[String] =
      new Bag[String](atoms.values.map((a) => a._1.symbol))
    if (!subElementBag.isSubbagOf(superElementBag))
      return false
    val superBondBag: Bag[(String, String)] =
      new Bag(bonds2.map((t) => (atoms2(t._1).head.asInstanceOf[String], atoms2(t._2).head.asInstanceOf[String])))
    val subBondBag: Bag[(String, String)] =
      new Bag(bonds.keys.map(t => (atoms(t._1)._1.symbol, atoms(t._2)._1.symbol)))
    if (!subBondBag.isSubbagOf(superBondBag))
      return false
    executeMatchingCommands(
      commands,
      atoms2.map(_.head.asInstanceOf[String]),
      bonds2.map((b)=>(Left(b._1), Left(b._2))),
      0)
  }

  def generateMatchingCommands(elements:List[String]):List[(Either[String,Int], Either[String,Int])] = {
    val elementsBag: Bag[String] = new Bag[String](elements)
    var bondsAndWeights: List[(Either[Int, Int], Either[Int, Int], Int)] =
      bonds.keys.toList
        .map((b: (Int, Int)) =>
          (b._1, elementsBag.occurrencesOf(atoms(b._1)._1.symbol),
            b._2, elementsBag.occurrencesOf(atoms(b._2)._1.symbol)))
        .map((b: (Int, Int, Int, Int)) =>
          if (b._2 <= b._4) (b._1, b._3, b._2 * b._4) else (b._3, b._1, b._2 * b._4))
        .map((b: (Int, Int, Int)) =>
          (Left(b._1): Left[Int, Int], Left(b._2): Left[Int, Int], b._3))
        .sortBy((b: (Either[Int, Int], Either[Int, Int], Int)) => b._3)

    def replace(before: Left[Int, Int], after: Right[Int, Int]):Unit = {
      bondsAndWeights = bondsAndWeights
        .map(b =>
          if (b._1 == before) (after, b._2, b._3 / (b._1 match {
            case Left(a) => elementsBag.occurrencesOf(atoms(a)._1.symbol)
            case _ => 0
          }))
          else if (b._2 == before) (b._1, after, b._3 / (b._2 match {
            case Left(a) => elementsBag.occurrencesOf(atoms(a)._1.symbol)
            case _ => 0
          }))
          else b)
        .sortBy((b: (Either[Int, Int], Either[Int, Int], Int)) => b._3)
    }

    var atomId: Int = 0
    var atom: Either[Int, Int] = Left(-1)
    var commands: List[(Either[String, Int], Either[String, Int])] = List()
    while (bondsAndWeights.nonEmpty) {
      var atom1:Either[Int, Int] = atom
      val bond = bondsAndWeights.find((triple) => triple._1 == atom1 || triple._2 == atom1) match {
        case Some(b) => b
        case None =>
          val b:(Either[Int, Int], Either[Int, Int], Int) = bondsAndWeights.head
          atom = b._1
          atom1 = atom
          atom1 match {
            case Left(a) =>
              atomId = atomId + 1
              replace(Left(a), Right(atomId))
              atom1 = Right(atomId)
            case Right(_) =>
          }
          (atom1, b._2, b._3)
      }
      bondsAndWeights = bondsAndWeights.filter(b => b._1 != bond._1 || b._2 != bond._2)
      var atom2: Either[Int, Int] =
        if (bond._1 == atom1)
          bond._2
        else
          bond._1
      commands = (
        atom match {case Left(a) => Left(atoms(a)._1.symbol) case Right(b) => Right(b)},
        atom2 match {case Left(a) => Left(atoms(a)._1.symbol) case Right(b) => Right(b)}) :: commands
      atom2 match {
        case Left(a) =>
          atomId = atomId + 1
          replace(Left(a), Right(atomId))
          atom2 = Right(atomId)
        case Right(_) =>
      }
      atom = atom2
    }
    commands.reverse
  }

  def executeMatchingCommands(
                               commands:List[(Either[String, Int], Either[String, Int])],
                               atoms:List[String],
                               bonds:List[(Either[Int, Int], Either[Int, Int])],
                               baseAtomId:Int):Boolean = {
    if (commands.isEmpty) return true
    def matchAtom(atom1:Either[String, Int], atom2:Either[Int, Int]):Boolean =
      atom1 match {
        case Left(a1) => atom2 match {case Left(a2)=> a1 == atoms(a2) case Right(_)=>false}
        case Right(a1) => atom2 match {case Left(_) => false case Right(a2)=> a1 == a2}
      }
    def replace(bs:List[(Either[Int, Int], Either[Int, Int])], before:Either[Int, Int], after:Either[Int, Int]):List[(Either[Int, Int], Either[Int, Int])] =
      bs.map((b:(Either[Int, Int], Either[Int, Int]))=> (if (b._1 == before) after else b._1, if (b._2 == before) after else b._2))
    val command = commands.head
    val from = command._1
    val to = command._2
    bonds.foreach((b:(Either[Int, Int], Either[Int, Int])) => {
      if (matchAtom(from, b._1) && matchAtom(to, b._2)) {
        var atomId:Int = baseAtomId
        var newBonds = bonds.filter(_ != b)
        b._1 match {
          case Left(_) =>
            atomId = atomId + 1
            newBonds = replace(newBonds, b._1, Right(atomId))
          case _ =>
        }
        b._2 match {
          case Left(_) =>
            atomId = atomId + 1
            newBonds = replace(newBonds, b._2, Right(atomId))
          case _ =>
        }
        if (executeMatchingCommands(commands.tail, atoms, newBonds, atomId))
          return true
      }
      if (matchAtom(from, b._2) && matchAtom(to, b._1)) {
        var atomId:Int = baseAtomId
        var newBonds = bonds.filter(_ != b)
        b._2 match {
          case Left(_) =>
            atomId = atomId + 1
            newBonds = replace(newBonds, b._2, Right(atomId))
          case _ =>
        }
        b._1 match {
          case Left(_) =>
            atomId = atomId + 1
            newBonds = replace(newBonds, b._1, Right(atomId))
          case _ =>
        }
        if (executeMatchingCommands(commands.tail, atoms, newBonds, atomId))
          return true
      }
    })
    false
  }

  preferredSize = new Dimension(400, 300)

}
