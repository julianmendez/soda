package soda.translator.io

trait LibraryDeployer
{

  import   java.io.File

  lazy val library_marker_file = "lib.soda"

  lazy val library_directory_in_jar = "/lib/soda/lib/"

  lazy val library_content_files: Seq [String] =
    SimpleFileReader_ ()
      .read_resource (library_directory_in_jar + "files.txt")
      .split ("\n")
      .toSeq

  def expand_library (lib_files: Seq [File]  ): Boolean =
    lib_files
      .map (lib_file => lib_file.getParent )
      .map (parent_directory => expand_files (parent_directory ) )
      .forall (x => x )

  def expand_files (parent_directory: String ): Boolean =
    library_content_files
      .map (lib_file_name =>
        SimpleFileWriter_ () .write_file (
          file = SimpleFileWriter_ () .create_file (parent_directory, lib_file_name ),
          content = SimpleFileReader_ () .read_resource (library_directory_in_jar + lib_file_name )
        ) )
      .forall (x => x )

}

case class LibraryDeployer_ ()
  extends
    LibraryDeployer
{

}
