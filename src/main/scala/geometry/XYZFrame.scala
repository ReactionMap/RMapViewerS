package geometry

import scala.collection.mutable

/**
 * Created by tomohiro on 15/02/09.
 */
class XYZFrame {
  val atoms: mutable.MutableList[String] = new mutable.MutableList[String]()
  val points: mutable.MutableList[Point] = new mutable.MutableList[Point]()

  def aboutEqualsTo(frame:XYZFrame):Boolean = {
    atoms.size == frame.atoms.size &&
      atoms.forall((a1:String)=>frame.atoms contains a1) &&
      points.forall((p1:Point)=>frame.points.exists((p2:Point)=> p1.dist(p2) < 1.0e-5))
  }

  def atom(i:Integer):String = atoms(i)
  def point(i:Integer):Point = points(i)

  def centerOfGravity:Point = points.reduce(_+_) / points.size

  def foreach(f:((String, Point)=>Unit)) = for (i <- 0 until atoms.size) f(atoms(i), points(i))

  def toXYZ:String = {
    atoms.size.toString + "\n\n" + (0 until atoms.size).map((i)=>atom(i)+"\t"+points(i).toXYZ).reduce(_+"\n"+_)+"\n"
  }

  def atomMap(frame:XYZFrame):Array[Int] = {
    val stderr:Double = 1.0
    if (atoms.size == 1) return Array(0)
    if (atoms.size == 2) {
      return if (atoms(0) == frame.atoms(0)) Array(0, 1) else Array(1, 0)
    }
    val myIndex1:Int = 0
    val myIndex2:Int = 1
    val myIndex3:Int = 2
    val myAtom1 = atoms(myIndex1)
    val myAtom2 = atoms(myIndex2)
    val myAtom3 = atoms(myIndex3)
    val myPoint1 = points(myIndex1)
    val myPoint2 = points(myIndex2)
    val myPoint3 = points(myIndex3)
    val mySqDist12 = myPoint1 squaredDist myPoint2
    val mySqDist23 = myPoint2 squaredDist myPoint3
    val mySqDist31 = myPoint3 squaredDist myPoint1
    var bestMap:Array[Int] = Array(0, 1, 2)
    var bestSSE:Double = Double.MaxValue
    for (hisIndex1 <- 0 until atoms.size) {
      val hisAtom1:String = frame.atom(hisIndex1)
      if (myAtom1 == hisAtom1) {
        val hisPoint1:Point = frame.point(hisIndex1)
        val possibleIndices3 = (0 until atoms.size).filter(
          (index) => Math.abs((hisPoint1 squaredDist frame.point(index)) - mySqDist31) < stderr)
        for (hisIndex2 <- 0 until atoms.size) {
          val hisAtom2:String = frame.atom(hisIndex2)
          val hisPoint2:Point = frame.point(hisIndex2)
          if (myAtom2 == hisAtom2
          && hisIndex1 != hisIndex2
          && Math.abs((hisPoint1 squaredDist hisPoint2) - mySqDist12) < stderr) {
            for (hisIndex3 <- possibleIndices3) {
              val hisAtom3:String = frame.atom(hisIndex3)
              val hisPoint3:Point = frame.point(hisIndex3)
              if (myAtom3 == hisAtom3
              && hisIndex1 != hisIndex3
              && hisIndex2 != hisIndex3
              && Math.abs((hisPoint2 squaredDist hisPoint3) - mySqDist23) < stderr) {
                val map = Array(hisIndex1, hisIndex2, hisIndex3)
                val transform:Point=>Point = this.transformation(frame, map)
                var sse:Double = 0.0
                for (i <- 0 until atoms.size) {
                  var bestIndex = 0
                  if (i < 3) {
                    bestIndex = map(i)
                  } else {
                    var minDist = Double.MaxValue
                    for (j <- 0 until atoms.size) {
                      val dist = points(i) squaredDist transform(frame.point(j))
                      if (dist < minDist) {
                        bestIndex = j
                        minDist = dist
                      }
                    }
                  }
                  sse += points(i).squaredDist(transform(frame.point(bestIndex)))
                }
                if (sse < bestSSE) {
                  bestSSE = sse
                  bestMap = map
                }
              }
            }
          }
        }
      }
    }
    bestMap
  }

  def transformation(frame:XYZFrame, map:Array[Int]):(Point=>Point) = {
    if (atoms.size <= 1) return (p)=>p
    val g1:Point = centerOfGravity
    val g2:Point = frame.centerOfGravity
    if (atoms.size == 2) {
      val xAxis1:Point = (points(1)-points(0)).unit
      val xAxis2:Point = (frame.point(map(1))-frame.point(map(0))).unit

      return (p)=> xAxis1 * (xAxis2 dot (p-g2)) + g1
    }
    val xAxis1:Point = (points(1) - points(0)).unit
    val zAxis1:Point = (xAxis1 cross ((points(2) - points(0)).unit)).unit
    val yAxis1:Point = (zAxis1 cross xAxis1).unit
    val xAxis2:Point = (frame.point(map(1)) - frame.point(map(0))).unit
    val yAxis2:Point = (frame.point(map(2)) - frame.point(map(0))).unit
    val zAxis2l:Point = (xAxis2 cross yAxis2).unit
    val yAxis2l:Point = (zAxis2l cross xAxis2).unit
    val zAxis2r:Point = (yAxis2 cross xAxis2).unit
    val yAxis2r:Point = (xAxis2 cross zAxis2r).unit
    val leftTransform:(Point=>Point) = (p)=>{
      val point:Point = p - g2
      val x = xAxis2 dot point
      val y = yAxis2l dot point
      val z = zAxis2l dot point
      xAxis1 * x + yAxis1 * y + zAxis1 * z + g1
    }
    val rightTransform:(Point=>Point) = (p)=>{
      val point:Point = p - g2
      val x = xAxis2 dot point
      val y = yAxis2r dot point
      val z = zAxis2r dot point
      xAxis1 * x + yAxis1 * y + zAxis1 * z + g1
    }
    var leftError:Double = 0.0
    var rightError:Double = 0.0
    for (index <- 0 until map.size) {
      leftError += points(index) squaredDist leftTransform(frame.point(map(index)))
      rightError += points(index) squaredDist rightTransform(frame.point(map(index)))
    }
    if (leftError <= 1.0e-5 || rightError >= 1.0e-5)
      leftTransform
    else
      rightTransform
  }

  def transformation(frame:XYZFrame):(Point=>Point) = {
    transformation(frame, atomMap(frame))
  }

  def tuples(tuples: List[Tuple2[String, Point]]) = {
    atoms ++= tuples.map(_._1)
    points ++= tuples.map(_._2)

  }

  def geometries(quadruples: List[Tuple4[String, Double, Double, Double]]) = {
    atoms ++= quadruples.map(_._1)
    points ++= quadruples.map((quad) => Point(quad._2, quad._3, quad._4))
  }

  def transformedBy(transformation:(Point=>Point)):XYZFrame = {
    val frame:XYZFrame = new XYZFrame()
    frame.atoms ++= atoms
    frame.points ++= points.map(transformation)
    frame
  }
}

object XYZFrame {
  def apply(geometry:List[List[Any]]) = {
    val frame:XYZFrame = new XYZFrame()
    for (line <- geometry) {
      frame.atoms += line(0).asInstanceOf[String]
      frame.points += Point(line(1).asInstanceOf[Double], line(2).asInstanceOf[Double], line(3).asInstanceOf[Double])
    }
    frame
  }

  def apply(source:String):XYZFrame = {
    val frame:XYZFrame = new XYZFrame()
    val lines:Iterator[String] = source.lines
    val size:Integer = lines.next().toInt
    lines.next()
    for (line <- lines) {
      if (!line.isEmpty) {
        val quad: Array[String] = line.split("\\s+")
        frame.atoms += quad(0)
        frame.points += Point(quad(1).toDouble, quad(2).toDouble, quad(3).toDouble)
      }
    }
    frame
  }

  def apply(source:String, transform:(Point=>Point)) = {
    val frame:XYZFrame = new XYZFrame()
    val lines:Iterator[String] = source.lines
    val size:Integer = lines.next().toInt
    lines.next()
    for (line <- lines) {
      val quad: Array[String] = line.split("\\s")
      frame.atoms += quad(0)
      frame.points += transform(Point(quad(1).toDouble, quad(2).toDouble, quad(3).toDouble))
    }
    frame
  }

  def main(args: Array[String]) {
    val frame1 = XYZFrame("3\n\nH\t0.0 1.0 1.0\nH\t2.0\t3.0\t4.0\nO -1.0 -1.0 -1.0\n")
    val frame2 = XYZFrame("3\n\nH\t1.0 2.0 2.0\nH\t3.0\t4.0\t5.0\nO 0.0 0.0 0.0\n")
    println(frame2.transformedBy(frame1.transformation(frame2)).toXYZ)
  }
}
