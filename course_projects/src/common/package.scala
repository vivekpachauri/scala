import java.io.File;
import java.io.FileInputStream

package object common {
  /**
   * `???` can be used for marking methods that remain to be implemented.
   *  @throws  An `Error`
   */
  def ??? : Nothing = throw new Error("an implementation is missing")

  type ??? = Nothing
  type *** = Any

  /**
   * Get a child of a file. For example,
   *
   *   subFile(homeDir, "b", "c")
   *
   * corresponds to ~/b/c
   */
  def subFile(file: File, children: String*) = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }

  /**
   * Get a resource from the `src/main/resources` directory. Eclipse does not copy
   * resources to the output directory, then the class loader cannot find them.
   */
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, ("src" :: "main" :: "resources" :: resourcePath): _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }

  val dictionaryPath = List("unsolved", "forcomp", "linuxwords.txt")

  def loadDictionary = {
    val wordstream = Option {
      println(dictionaryPath mkString "/")
      getClass.getClassLoader.getResourceAsStream(dictionaryPath mkString ("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    }
    catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    }
    finally {
      wordstream.close()
    }
  }

}
