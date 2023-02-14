package soda.translator.io

/*
 * This package contains all the classes for I/O communication.
 * It is also the entry point for the translator.
 */





/**
 * This class is used to scan files in a given directory.
 */

trait DirectoryScanner
{

  import   java.io.File

  def get_all_files (start : File) : Seq [File] =
    if ( start.isFile
    ) Seq (start)
    else _scan (Seq () ) (start.listFiles ().toSeq)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_scan (found : Seq [File] ) (to_scan : Seq [File] ) : Seq [File] =
    if ( to_scan.isEmpty
    ) found
    else _tailrec_scan (found.+: (to_scan.head) ) (_get_files_to_scan (to_scan) )

  private def _scan (found : Seq [File] ) (to_scan : Seq [File] ) : Seq [File] =
    _tailrec_scan (found) (to_scan)

  private def _get_files_to_scan (to_scan : Seq [File] ) : Seq [File] =
    if ( to_scan.isEmpty
    ) to_scan
    else _get_files_to_scan_with (to_scan.head) (to_scan.tail)

  private def _get_files_to_scan_with (to_scan_head : File) (to_scan_tail : Seq [File] ) : Seq [File] =
    if ( to_scan_head.isDirectory
    ) to_scan_tail.++ (to_scan_head.listFiles () )
    else to_scan_tail

}

case class DirectoryScanner_ () extends DirectoryScanner
