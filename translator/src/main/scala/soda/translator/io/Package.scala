package soda.translator.io

/*
 * This package contains all the classes for I/O communication.
 * It is also the entry point for the translator.
 */



trait Package

trait AbstractDirectoryProcessor
{

  def   start : String
  def   process_soda_file : java.io.File => Boolean

}

case class AbstractDirectoryProcessor_ (start : String, process_soda_file : java.io.File => Boolean) extends AbstractDirectoryProcessor

object AbstractDirectoryProcessor {
  def mk (start : String) (process_soda_file : java.io.File => Boolean) : AbstractDirectoryProcessor =
    AbstractDirectoryProcessor_ (start, process_soda_file)
}

trait DirectoryProcessor
  extends
    AbstractDirectoryProcessor
{

  def   start : String
  def   process_soda_file : java.io.File => Boolean

  import   java.io.File

  lazy val soda_suffix = ".soda"

  lazy val package_file_name = "Package.soda"

  private lazy val _all_files : Seq [File] =
    DirectoryScanner_ () .get_all_files ( new File (start) )

  private lazy val _sorted_all_soda_files : Seq [File] =
    _all_files
      .filter ( x => x .isFile)
      .filter ( x => x .getName .endsWith (soda_suffix) )
      .distinct
      .sorted

  private lazy val _sorted_package_files : Seq [File] =
    _all_files
      .filter ( x => x .isFile)
      .filter ( x => x .getName == package_file_name)
      .distinct
      .sorted

  private lazy val _sorted_soda_directories : Seq [File] =
    _sorted_all_soda_files
      .map ( x => x .getParentFile)
      .distinct
      .sorted

  private lazy val _sorted_package_file_directories : Seq [File] =
    _sorted_package_files
      .map ( x => x .getParentFile)
      .distinct
      .sorted

  private lazy val _sorted_soda_non_package_files : Seq [File] =
    _sorted_all_soda_files
      .filter ( x => ! (x .getName == package_file_name) )

  private lazy val _original_soda_files : Seq [File] =
    _sorted_package_files  .++ (_sorted_soda_non_package_files)

  private lazy val _missing_package_files : Seq [File] =
    _sorted_soda_directories
      .filter ( x => ! (_sorted_package_file_directories .contains (x) ) )
      .map ( x => new File (x , package_file_name) )

  private lazy val _soda_files_and_missing_package_files : Seq [File] =
    _missing_package_files .++ (_original_soda_files)

  private lazy val _sorted_lib_files : Seq [File] =
    _all_files
      .filter ( x => x .isFile)
      .filter ( file => file .getName == LibraryDeployer_ () .library_marker_file)
      .sorted

  private def _expand_library_files (ready : Boolean) : Boolean =
    ready &&
      LibraryDeployer_ () .expand_library (_sorted_lib_files)

  private def _add_missing_package_files (ready : Boolean) : Boolean =
    ready &&
      _missing_package_files
       .map ( x => x .createNewFile)
       .forall ( x => x)

  private def _process_all_soda_files (ready : Boolean) : Boolean =
    ready &&
      _soda_files_and_missing_package_files
        .map (process_soda_file)
        .forall ( x => x)

  def process () : Boolean =
    Some (true)
      .map (_expand_library_files)
      .map (_add_missing_package_files)
      .map (_process_all_soda_files)
      .getOrElse (false)

}

case class DirectoryProcessor_ (start : String, process_soda_file : java.io.File => Boolean) extends DirectoryProcessor

object DirectoryProcessor {
  def mk (start : String) (process_soda_file : java.io.File => Boolean) : DirectoryProcessor =
    DirectoryProcessor_ (start, process_soda_file)
}


/**
 * This class is used to scan files in a given directory.
 */

trait DirectoryScanner
{

  import   java.io.File

  private def _get_files_to_scan_with (to_scan_head : File) (to_scan_tail : Seq [File] ) : Seq [File] =
    if ( to_scan_head .isDirectory
    ) to_scan_tail .++ (_list_files (to_scan_head) )
    else to_scan_tail

  private def _get_files_to_scan (to_scan : Seq [File] ) : Seq [File] =
    if ( to_scan .isEmpty
    ) to_scan
    else _get_files_to_scan_with (to_scan .head) (to_scan .tail)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_scan (found : Seq [File] ) (to_scan : Seq [File] ) : Seq [File] =
    if ( to_scan .isEmpty
    ) found
    else _tailrec_scan (found .+: (to_scan .head) ) (_get_files_to_scan (to_scan) )

  private def _scan (found : Seq [File] ) (to_scan : Seq [File] ) : Seq [File] =
    _tailrec_scan (found) (to_scan)

  private def _list_files_with (files : Option [Array [File] ] ) : Seq [File] =
    files match  {
      case Some (array) => array .toSeq
      case otherwise => Seq ()
    }

  private def _list_files (to_scan_head : File) : Seq [File] =
    _list_files_with (Option (to_scan_head .listFiles) )

  def get_all_files (start : File) : Seq [File] =
    if ( start .isFile
    ) Seq (start)
    else _scan (Seq () ) (start .listFiles () .toSeq)

}

case class DirectoryScanner_ () extends DirectoryScanner

object DirectoryScanner {
  def mk : DirectoryScanner =
    DirectoryScanner_ ()
}


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

  private def _expand_files (parent_directory : String) : Boolean =
    _library_content_files
      .map ( lib_file_name =>
        SimpleFileWriter_ () .write_file_with (
          file = SimpleFileWriter_ () .create_file (parent_directory) (lib_file_name) ) (
          content = SimpleFileReader_ () .read_resource (
            _library_directory_in_jar + lib_file_name)
        ) )
      .forall ( x => x)

  def expand_library (lib_files : Seq [File] ) : Boolean =
    lib_files
      .map ( lib_file => lib_file .getParent)
      .map ( parent_directory => _expand_files (parent_directory) )
      .forall ( x => x)

}

case class LibraryDeployer_ () extends LibraryDeployer

object LibraryDeployer {
  def mk : LibraryDeployer =
    LibraryDeployer_ ()
}


/**
 * This is an auxiliary class to read small files.
 */

trait SimpleFileReader
{

  import   java.io.BufferedReader
  import   java.io.InputStream
  import   java.io.InputStreamReader
  import   java.nio.file.Files
  import   java.nio.file.Paths
  import   java.util.stream.Collectors

  lazy val new_line = "\n"

  def read_file (file_name : String) : String =
    new String (Files .readAllBytes (Paths .get (file_name) ) )

  def read_resource (file_name : String) : String =
    read_input_stream (getClass .getResourceAsStream (file_name) )

  def read_input_stream (input_stream : InputStream) : String =
    _read_reader_content ( new BufferedReader ( new InputStreamReader (input_stream) ) )

  private def _read_reader_content (reader : BufferedReader) : String =
    reader .lines () .collect (Collectors .joining (new_line) )

}

case class SimpleFileReader_ () extends SimpleFileReader

object SimpleFileReader {
  def mk : SimpleFileReader =
    SimpleFileReader_ ()
}

/**
 * This is an auxiliary class to write small files.
 */

trait SimpleFileWriter
{

  import   soda.lib.SomeSD_
  import   java.io.File
  import   java.io.FileWriter
  import   java.io.Writer

  private def _write_content (writer : Writer) (content : String) : Boolean =
    SomeSD_ (true)
      .map ( x => writer .write (content) )
      .map ( x => writer .flush () )
      .map ( x => writer .close () )
      .map ( x => true)
      .getOrElse (false)

  def write_file_with (file : File) (content : String) : Boolean =
    _write_content (new FileWriter (file , false) ) (content)

  def write_file (file_name : String) (content : String) : Boolean =
    write_file_with (new File (file_name) ) (content)

  def create_file (parent_directory : String) (file_name : String) : File =
    new File (parent_directory , file_name)

  def append_file_with (file : File) (content : String) : Boolean =
    _write_content (new FileWriter (file , true) ) (content)

  def append_file (file_name : String) (content : String) : Boolean =
    append_file_with (new File (file_name) ) (content)

}

case class SimpleFileWriter_ () extends SimpleFileWriter

object SimpleFileWriter {
  def mk : SimpleFileWriter =
    SimpleFileWriter_ ()
}

