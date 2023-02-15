package soda.translator.io

/*
 * This package contains all the classes for I/O communication.
 * It is also the entry point for the translator.
 */



trait Package
/**
 * This class is used to scan files in a given directory.
 */

trait DirectoryScanner
{

  import   java.io.File

  def get_all_files (start : File) : Seq [File] =
    if ( start.isFile
    ) Seq (start)
    else _scan (Seq () ) (start.listFiles ().toSeq)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_scan (found : Seq [File] ) (to_scan : Seq [File] ) : Seq [File] =
    if ( to_scan.isEmpty
    ) found
    else _tailrec_scan (found.+: (to_scan.head) ) (_get_files_to_scan (to_scan) )

  private def _scan (found : Seq [File] ) (to_scan : Seq [File] ) : Seq [File] =
    _tailrec_scan (found) (to_scan)

  private def _get_files_to_scan (to_scan : Seq [File] ) : Seq [File] =
    if ( to_scan.isEmpty
    ) to_scan
    else _get_files_to_scan_with (to_scan.head) (to_scan.tail)

  private def _get_files_to_scan_with (to_scan_head : File) (to_scan_tail : Seq [File] ) : Seq [File] =
    if ( to_scan_head.isDirectory
    ) to_scan_tail.++ (to_scan_head.listFiles () )
    else to_scan_tail

}

case class DirectoryScanner_ () extends DirectoryScanner
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
    new String (Files.readAllBytes (Paths.get (file_name) ) )

  def read_resource (file_name : String) : String =
    read_input_stream (getClass.getResourceAsStream (file_name) )

  def read_input_stream (input_stream : InputStream) : String =
    _read_reader_content ( new BufferedReader ( new InputStreamReader (input_stream) ) )

  private def _read_reader_content (reader : BufferedReader) : String =
    reader.lines ().collect (Collectors.joining (new_line) )

}

case class SimpleFileReader_ () extends SimpleFileReader

/**
 * This is an auxiliary class to write small files.
 */

trait SimpleFileWriter
{

  import   soda.lib.SomeSD_
  import   java.io.File
  import   java.io.FileWriter
  import   java.io.Writer

  def write_file (file_name : String) (content : String) : Boolean =
    write_file_with ( new File (file_name) ) (content)

  def write_file_with (file : File) (content : String) : Boolean =
    _write_content ( new FileWriter (file , false) ) (content)

  def append_file (file_name : String) (content : String) : Boolean =
    append_file_with ( new File (file_name) ) (content)

  def append_file_with (file : File) (content : String) : Boolean =
    _write_content ( new FileWriter (file , true) ) (content)

  private def _write_content (writer : Writer) (content : String) : Boolean =
    SomeSD_ (true)
      .map (  x => writer.write (content) )
      .map (  x => writer.flush () )
      .map (  x => writer.close () )
      .map (  x => true )
      .getOrElse (false)

  def create_file (parent_directory : String) (file_name : String) : File =
    new File (parent_directory, file_name)

}

case class SimpleFileWriter_ () extends SimpleFileWriter
