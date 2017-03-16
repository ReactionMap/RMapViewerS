package utilities

/**
 * Created by tomohiro on 2014/08/01.
 */
object XYZUtils {
  def printStaticXYZ(json:List[List[Any]]):String = {
    json.length.toString + json.map(printXYZLine).foldLeft("\n")((line1, line2)=>line1+"\n"+line2)
  }
  def printXYZLine(atomGeom:List[Any]):String = {
   atomGeom.head.asInstanceOf[String] + atomGeom.tail.map((x)=>x.toString).foldLeft(" ")((x1,x2)=>x1+" "+x2)
  }
}
