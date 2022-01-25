package soda.translator.io

/**
 * This class is used to scan files in a given directory.
 */
trait DirectoryScanner {

  import   java.io.File

  def get_all_files (start: File ): Seq [File] =
    if (start.isFile
    ) Seq (start )
    else scan (Seq (), start.listFiles () .toSeq )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_scan (found: Seq [File], to_scan: Seq [File]  ): Seq [File] =
    if (to_scan.isEmpty
    ) found
    else _tailrec_scan (found.+: (to_scan.head ), get_files_to_scan (to_scan )  )

  def scan (found: Seq [File], to_scan: Seq [File]  ): Seq [File] =
    _tailrec_scan (found, to_scan )

  def get_files_to_scan (to_scan: Seq [File]  ): Seq [File] =
    if (to_scan.head.isDirectory
    ) to_scan.tail.++ (to_scan.head.listFiles ()  )
    else to_scan.tail

}

case class DirectoryScanner_ () extends DirectoryScanner
