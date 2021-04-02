package scopus.translator.io

import java.io.File

/**
 * This class is used to scan files in a given directory.
 */
case class DirectoryScanner (  ) {

  lazy val Scopus_suffix = ".scopus"

  def get_all_files ( start: File ) : Seq [ File ] =
    if ( start.isFile
    ) Seq ( start )
    else scan ( Seq (  )  , start.listFiles (  ) .toSeq )

  def get_scopus_files ( start: File ) : Seq [ File ] =
    get_all_files ( start )
      .filter ( x => x.isFile )
      .filter ( x => x.getName.endsWith ( Scopus_suffix )  )

  def scan ( found: Seq [ File ]  , to_scan: Seq [ File ]  ) : Seq [ File ] = {
    lazy val result = rec ( found , to_scan )

    import scala.annotation.tailrec
        @tailrec
    def rec ( found: Seq [ File ]  , to_scan: Seq [ File ]  ) : Seq [ File ] =
      if ( to_scan.isEmpty
      ) found
      else {
        lazy val next_file = to_scan.head
        lazy val remaining = to_scan.tail
        lazy val new_to_scan =
          if ( next_file.isDirectory
          ) remaining.++ ( next_file.listFiles (  )  )
          else remaining
        rec ( found.+: ( next_file )  , new_to_scan )
      }

    result
  }

}
