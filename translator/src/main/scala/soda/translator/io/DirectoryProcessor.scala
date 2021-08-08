package soda.translator.io


trait DirectoryProcessor {
  import java.io.File

  def start: String

  def process_soda_file: File => Boolean

  lazy val soda_suffix = ".soda"

  lazy val all_files =
    DirectoryScanner_ () .get_all_files (new File (start )  )

  lazy val soda_files =
    all_files
      .filter (x => x.isFile )
      .filter (x => x.getName.endsWith (soda_suffix )  )

  lazy val lib_files =
    all_files
      .filter (x => x.isFile )
      .filter (file => file.getName == LibraryDeployer_ () .library_marker_file )

  def process (): Boolean =
    LibraryDeployer_ () .expand_library (lib_files ) &&
      soda_files
        .map (process_soda_file )
        .forall (x => x )
}

case class DirectoryProcessor_ (start: String, process_soda_file: java.io.File => Boolean )  extends DirectoryProcessor
