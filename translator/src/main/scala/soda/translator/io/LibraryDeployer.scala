package soda.translator.io


case class LibraryDeployer () {
  import java.io.File

  lazy val Library_marker_file = "lib.soda"
  lazy val Library_directory_in_jar = "/lib/soda/lib/"
  lazy val Library_content_files: Seq [String] =
    SimpleIO ()
      .read_resource (Library_directory_in_jar + "files.txt")
      .split ("\n")
      .toSeq

  def expand_library (lib_files: Seq [File]  ): Boolean =
    lib_files
      .map (lib_file => lib_file.getParent )
      .map (parent_directory =>
        Library_content_files
          .map (lib_file_name =>
            SimpleIO () .write_file (file = SimpleIO () .create_file (parent_directory, lib_file_name ), content = SimpleIO () .read_resource (Library_directory_in_jar + lib_file_name )            )          ) .forall (x => x )      ) .forall (x => x )
}
