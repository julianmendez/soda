package soda.translator.io

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

  def read_file (file_name: String ): String =
    new String (Files.readAllBytes (Paths.get (file_name )  )  )

  def read_resource (file_name: String ): String =
    read_input_stream (getClass.getResourceAsStream (file_name )  )

  def read_input_stream (input_stream: InputStream ): String =
    read_reader_content (new BufferedReader (new InputStreamReader (input_stream ) ) )

  def read_reader_content (reader: BufferedReader ): String =
    reader.lines () .collect (Collectors.joining (new_line )  )

}

/**
 * This is an auxiliary class to write small files.
 */

case class SimpleFileReader_ ()
  extends
    SimpleFileReader
{

}

trait SimpleFileWriter
{

  import   soda.lib.SomeSD_
  import   java.io.File
  import   java.io.FileWriter
  import   java.io.Writer

  def write_file (file_name: String, content: String ): Boolean =
    write_file (new File (file_name ), content )

  def write_file (file: File, content: String ): Boolean =
    _write_content (new FileWriter (file ), content )

  def _write_content (writer: Writer, content: String ): Boolean =
    SomeSD_ (true )
      .map (x => writer.write (content ) )
      .map (x => writer.flush () )
      .map (x => writer.close () )
      .map (x => true )
      .value

  def create_file (parent_directory: String, file_name: String ): File =
    new File (parent_directory, file_name )

}

case class SimpleFileWriter_ ()
  extends
    SimpleFileWriter
{

}
