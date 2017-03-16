package geometry

import atom.Atom

/**
  * Created by tomohiro on 2016/02/25.
  */
class StructureSignature(geometry:List[List[Any]], bond:List[(Int, Int, Int)]) {
  val atoms:List[Atom] = geometry.map(_(0).asInstanceOf[String]).map(Atom(_).get)
  val value:List[Int] = atoms.indices
    .map((atomIndex:Int) =>
      bond
        .filter((triple:(Int,Int,Int))=> triple._1 == atomIndex || triple._2 == atomIndex)
        .map((triple:(Int,Int,Int))=> atoms(triple._1).prime * atoms(triple._2).prime)
        .product)
    .sorted
    .toList
  def ==(sig:StructureSignature):Boolean = value == sig.value
}
