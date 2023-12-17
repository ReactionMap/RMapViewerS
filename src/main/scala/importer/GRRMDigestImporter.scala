package importer

import java.io.File

class GRRMDigestImporter(dirname: File) extends GRRMImporter(dirname: File, false) {
  override def readDC(): List[DC] = List.empty
}
