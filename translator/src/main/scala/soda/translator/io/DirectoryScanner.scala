package soda.translator.io


/**
 * This class is used to scan files in a given directory.
 */
case class DirectoryScanner () {
  import java.io.File

  def get_all_files (start: File ): Seq [File] =
    if (start.isFile
    ) Seq (start )
    else RecursiveScanner (Seq (), start.listFiles () .toSeq ) .scan ()

  case class RecursiveScanner (found: Seq [File], to_scan: Seq [File]  ) {

    def scan (): Seq [File] = {
      lazy val result = rec (found, to_scan )

      import scala.annotation.tailrec
        @tailrec
      def rec (found: Seq [File], to_scan: Seq [File]  ): Seq [File] =
        if (to_scan.isEmpty
        ) found
        else rec (found.+: (to_scan.head ), get_files_to_scan (to_scan )  )

      result
    }

    def get_files_to_scan (to_scan: Seq [File]  ): Seq [File] =
      if (to_scan.head.isDirectory
      ) to_scan.tail.++ (to_scan.head.listFiles ()  )
      else to_scan.tail
  }
}
