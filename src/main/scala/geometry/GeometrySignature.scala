package geometry

/**
  * Created by tomohiro on 2016/02/22.
  */
class GeometrySignature(geometry:List[List[Any]]) {
  val signature:Map[(String,String),List[Double]] =
    geometry.foldLeft(Map.empty[(String,String),List[Double]])((map1, list1) => {
      val atom1 = list1(0).asInstanceOf[String]
      val point1 = new Point(
        list1(1).asInstanceOf[Double],
        list1(2).asInstanceOf[Double],
        list1(3).asInstanceOf[Double])
      geometry.foldLeft(map1)((map2, list2)=> {
        if (list1 == list2) {
          map2
        } else {
          val atom2 = list2(0).asInstanceOf[String]
          val point2 = new Point(
            list2(1).asInstanceOf[Double],
            list2(2).asInstanceOf[Double],
            list2(3).asInstanceOf[Double])
          val key = if (atom1 <= atom2) (atom1, atom2) else (atom2, atom1)
          val dist = point1.dist(point2)
          if (map2.isDefinedAt(key))
            map2.updated(key, dist::map2(key) )
          else
            map2 + (key -> List(dist))
        }
      })
    }).mapValues(_.sorted)
  def dist(peer:GeometrySignature):Double = {
    val mySignature = this.signature
    val hisSignature = peer.signature
    mySignature.keys.map((key:(String,String))=> {
      val myDists = mySignature(key)
      hisSignature.get(key) match {
        case Some(dists) =>
          (0 until Math.min(dists.length, myDists.length)).
            map((i:Int)=>Math.abs(myDists(i)-dists(i))).
            reduce(_+_)+Math.abs(dists.length-myDists.length)
        case None => myDists.length
      }
    }).fold(0.0)(_+_)+
      hisSignature.keys.map((key:(String,String))=> mySignature.get(key) match {
        case Some(dists) => dists.length
        case None => 0.0
      }).reduce(_+_)
  }
}
