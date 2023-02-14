package soda.translator.io

/*
 * This package contains all the classes for I/O communication.
 * It is also the entry point for the translator.
 */





trait LibraryDeployer
{

  import   java.io.File

  lazy val library_marker_file = "lib.soda"

  private lazy val _library_directory_in_jar = "/lib/soda/lib/"

  private lazy val _library_content_files : Seq [String] =
    SimpleFileReader_ ()
      .read_resource (_library_directory_in_jar + "files.txt")
      .split ("\n")
      .toSeq

  def expand_library (lib_files : Seq [File] ) : Boolean =
    lib_files
      .map (  lib_file => lib_file.getParent)
      .map (  parent_directory => _expand_files (parent_directory) )
      .forall (  x => x)

  private def _expand_files (parent_directory : String) : Boolean =
    _library_content_files
      .map (  lib_file_name =>
        SimpleFileWriter_ ().write_file_with (
          file = SimpleFileWriter_ ().create_file (parent_directory) (lib_file_name) ) (
          content = SimpleFileReader_ ().read_resource (_library_directory_in_jar + lib_file_name)
        ) )
      .forall (  x => x)

}

case class LibraryDeployer_ () extends LibraryDeployer
