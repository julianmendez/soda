package soda.translator.io

import java.io.File

case class DirectoryProcessor (start: String, process_soda_file: File => Boolean ) {

  lazy val Soda_suffix = ".soda"

  def process (): Boolean =
    LibraryDeployer () .expand_library (lib_files ) &&
      soda_files
        .map (file => process_soda_file (file )  )
        .forall (x => x )

  lazy val all_files = DirectoryScanner () .get_all_files (new File (start )  )

  lazy val soda_files =
    all_files
      .filter (x => x.isFile )
      .filter (x => x.getName.endsWith (Soda_suffix )  )

  lazy val lib_files =
    all_files
      .filter (x => x.isFile )
      .filter (file => file.getName == LibraryDeployer () .Library_marker_file )

}
