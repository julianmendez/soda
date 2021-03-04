package scopus.translator

import java.io.File

case class DirectoryScanner() {

  val Scopus_suffix = ".scopus"

  def get_all_files(start: File): Seq[File] =
    if ( start.isFile
    ) Seq(start)
    else _scan(Seq(), start.listFiles().toSeq)

  def get_scopus_files(start: File): Seq[File] =
    get_all_files(start)
      .filter(x => x.isFile)
      .filter(x => x.getName.endsWith(Scopus_suffix))

  import scala.annotation.tailrec
  @tailrec final
  def _scan(found: Seq[File], to_scan: Seq[File]): Seq[File] =
    if ( to_scan.isEmpty
    ) found
    else {
      val next_file = to_scan.head
      val remaining = to_scan.tail
      val new_to_scan =
        if ( next_file.isDirectory
        ) remaining.++(next_file.listFiles())
        else remaining
      _scan(found.+:(next_file), new_to_scan)
    }

}
