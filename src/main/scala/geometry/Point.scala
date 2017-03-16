package geometry

/**
 * Created by tomohiro on 2014/07/31.
 */
case class Point(x:Double, y:Double, z:Double = 0.0) {
  def r:Double = Math.sqrt(x * x + y * y + z * z)
  def theta:Double = {
    if (Math.abs(x) < 1.0e-8) {
      if (Math.abs(y) < 1.0e-8) {
        0.0
      } else {
        if (y >= 0.0) {
          Math.PI * 0.5
        } else {
          Math.PI * 1.5
        }
      }
    } else {
      val theta = Math.atan(y / x)
      if (x >= 0.0) {
        if (y >= 0.0) {
          theta
        } else {
          2.0 * Math.PI + theta
        }
      } else {
        Math.PI + theta
      }
    }
  }
  def +(p:Point):Point = Point(x+p.x, y+p.y, z+p.z)
  def -(p:Point):Point = Point(x-p.x, y-p.y, z-p.z)
  def *(n:Double):Point = Point(x*n, y*n, z*n)
  def /(n:Double):Point = Point(x/n, y/n, z/n)
  def cross(p:Point):Point = Point(y*p.z - z*p.y, z*p.x - x*p.z, x*p.y - y*p.x)
  def dot(p:Point):Double = x*p.x + y*p.y + z*p.z
  def dist(p:Point):Double = Math.sqrt(this.squaredDist(p))
  def squaredDist(p:Point):Double = {
    val dx = x-p.x
    val dy = y-p.y
    val dz = z-p.z
    dx * dx + dy * dy + dz * dz
  }
  def unit:Point = this / r
  def toXYZ:String = x.toString+"\t"+y.toString+"\t"+z.toString
}
