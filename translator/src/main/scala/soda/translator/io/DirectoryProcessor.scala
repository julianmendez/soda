package soda.translator.io


trait DirectoryProcessor {
  import java.io.File

  def start: String

  def process_soda_file: File => Boolean

  lazy val soda_suffix = ".soda"

  lazy val all_files = DirectoryScannerImpl () .get_all_files (new File (start )  )

  lazy val soda_files =
    all_files
      .filter (x => x.isFile )
      .filter (x => x.getName.endsWith (soda_suffix )  )

  lazy val lib_files =
    all_files
      .filter (x => x.isFile )
      .filter (file => file.getName == LibraryDeployerImpl () .library_marker_file )

  def process (): Boolean =
    LibraryDeployerImpl () .expand_library (lib_files ) &&
      soda_files
        .map (process_soda_file )
        .forall (x => x )
}

case class DirectoryProcessorImpl (start: String, process_soda_file: java.io.File => Boolean )  extends DirectoryProcessor
