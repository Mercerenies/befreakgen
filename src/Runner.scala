
package com.mercerenies.befreak

import scala.sys.process._
import java.io.{BufferedWriter, FileWriter}
import java.nio.file.Path

object Runner:

  val path = "~/Documents/eulers-melting-pot/vendor/befreak/bfi"

  def runFile(filename: String | Path): String =
    s"$path $filename".!!

  def runCode(code: String): String =
    Temporary.withTmp("__befreak_", ".tmp") { (path) =>
      val bw = BufferedWriter(FileWriter(path.toFile))
      try
        bw.write(code)
      finally
        bw.close()
      runFile(path)
    }

end Runner
