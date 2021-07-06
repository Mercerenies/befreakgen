
package com.mercerenies.befreak

import java.nio.file.{Path, Files}

object Temporary:

  def withTmp[B](prefix: String | Null, suffix: String | Null)(block: (Path) => B): B =
    val tempFile = Files.createTempFile(prefix, suffix).nn
    try
      block(tempFile)
    finally
      Files.delete(tempFile)

end Temporary
