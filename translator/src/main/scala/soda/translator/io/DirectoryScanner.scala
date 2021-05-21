package soda.translator.io

import java.io.File

/**
 * This class is used to scan files in a given directory.
 */
case class DirectoryScanner () {

  def get_all_files (start: File ): Seq [File] =
    if (start.isFile
    ) Seq (start )
    else scan (Seq (), start.listFiles () .toSeq )

  def scan (found: Seq [File], to_scan: Seq [File]  ): Seq [File] = {
    lazy val result = rec (found, to_scan )

    import scala.annotation.tailrec
        @tailrec
    def rec (found: Seq [File], to_scan: Seq [File]  ): Seq [File] =
      if (to_scan.isEmpty
      ) found
      else rec (found.+: (to_scan.head ), new_to_scan (to_scan )  )

    def new_to_scan (to_scan: Seq [File]  ): Seq [File] =
      if (to_scan.head.isDirectory
      ) to_scan.tail.++ (to_scan.head.listFiles ()  )
      else to_scan.tail

    result
  }

}
