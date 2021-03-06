// See LICENSE for license details.

package firrtl.options

import java.io.File

/** Options that every stage shares
  * @param targetDirName a target (build) directory
  * @param an input annotation file
  */
final case class StageOptions(
  targetDir:       String      = TargetDirAnnotation().dir,
  annotationFiles: Seq[String] = Seq.empty ) {

  /** Generate a filename (with an optional suffix) and create any parent directories. Suffix is only added if it is not
    * already there.
    * @param filename the name of the file
    * @param suffix an optional suffix that the file must end in
    * @return the name of the file
    * @note the filename may include a path
    * @throws IllegalArgumentException if the filename is empty or if the suffix doesn't start with a '.'
    */
  def getBuildFileName(filename: String, suffix: Option[String] = None): String = {
    require(filename.nonEmpty, "requested filename must not be empty")
    require(suffix.isEmpty || suffix.get.startsWith("."), s"suffix must start with '.', but got ${suffix.get}")

    /* Mangle the file in the following ways:
     *   1. Ensure that the file ends in the requested suffix
     *   2. Prepend the target directory if this is not an absolute path
     */
    val file = {
      val f = if (suffix.nonEmpty && !filename.endsWith(suffix.get)) {
        new File(filename + suffix.get)
      } else {
        new File(filename)
      }
      if (f.isAbsolute) {
        f
      } else {
        new File(targetDir + "/" + f)
      }
    }.getCanonicalFile

    val parent = file.getParentFile

    if (!parent.exists) { parent.mkdirs() }

    file.toString
  }

}
