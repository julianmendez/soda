package soda.translator.io


/**
 * This is an auxiliary class to read and write small files.
 */
case class SimpleIO () {
  import java.io.BufferedReader
  import java.io.File
  import java.io.FileWriter
  import java.io.InputStreamReader
  import java.nio.file.Files
  import java.nio.file.Paths
  import java.util.stream.Collectors

  lazy val New_line = "\n"

  def read_file (file_name: String ): String =
    new String (Files.readAllBytes (Paths.get (file_name )  )  )

  def read_resource (file_name: String ): String = {
    lazy val input_stream = getClass.getResourceAsStream (file_name )
    lazy val reader = new BufferedReader (new InputStreamReader (input_stream )  )
    reader.lines () .collect (Collectors.joining (New_line )  )
  }

  def write_file (file_name: String, content: String ): Boolean =
    write_file (new File (file_name ), content )

  def write_file (file: File, content: String ): Boolean = {
    lazy val writer = new FileWriter (file )
    writer.write (content )
    writer.flush ()
    writer.close ()
    true
  }

  def create_file (parent_directory: String, file_name: String ): File =
    new File (parent_directory, file_name )

}
