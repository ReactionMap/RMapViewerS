package graph

import java.io.File

class GRRMDigestImporter(dirname: File, comfilename: String) extends GRRMImporter(dirname: File, comfilename: String, false) {
  override def readDC(): List[DC] = List.empty
}
