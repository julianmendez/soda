package soda.translator.io

/*
 * This package contains all the classes for I/O communication.
 * It is also the entry point for the translator.
 */





trait AbstractDirectoryProcessor
{

  def   start : String
  def   process_soda_file : java.io.File => Boolean

}

case class AbstractDirectoryProcessor_ (start : String, process_soda_file : java.io.File => Boolean) extends AbstractDirectoryProcessor

trait DirectoryProcessor
  extends
    AbstractDirectoryProcessor
{

  def   start : String
  def   process_soda_file : java.io.File => Boolean

  import   java.io.File

  lazy val soda_suffix = ".soda"

  lazy val package_file_name = "Package.soda"

  private lazy val _all_files =
    DirectoryScanner_ ().get_all_files ( new File (start))

  private lazy val _all_soda_files =
    _all_files
      .filter (  x => x.isFile)
      .filter (  x => x.getName.endsWith (soda_suffix))

  private lazy val _package_files =
    _all_files
      .filter (  x => x.isFile)
      .filter (  x => x.getName == package_file_name)

  private lazy val _soda_non_package_files =
    _all_soda_files
      .filter (  x => ! (x.getName == package_file_name) )

  private lazy val _soda_files =
    _package_files .++ (_soda_non_package_files)

  private lazy val _lib_files =
    _all_files
      .filter (  x => x.isFile)
      .filter (  file => file.getName == LibraryDeployer_ ().library_marker_file)

  def process () : Boolean =
    LibraryDeployer_ ().expand_library (_lib_files) &&
      _soda_files
        .map (process_soda_file)
        .forall (  x => x)

}

case class DirectoryProcessor_ (start : String, process_soda_file : java.io.File => Boolean) extends DirectoryProcessor
