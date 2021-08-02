package soda.translator.io


trait LibraryDeployer {
  import java.io.File

  lazy val library_marker_file = "lib.soda"

  lazy val library_directory_in_jar = "/lib/soda/lib/"

  lazy val library_content_files: Seq [String] =
    SimpleIOImpl ()
      .read_resource (library_directory_in_jar + "files.txt")
      .split ("\n")
      .toSeq

  def expand_library (lib_files: Seq [File]  ): Boolean =
    lib_files
      .map (lib_file => lib_file.getParent )
      .map (parent_directory =>
        library_content_files
          .map (lib_file_name =>
            SimpleIOImpl () .write_file (file = SimpleIOImpl () .create_file (parent_directory, lib_file_name ), content = SimpleIOImpl () .read_resource (library_directory_in_jar + lib_file_name )            )          ) .forall (x => x )      ) .forall (x => x )
}

case class LibraryDeployerImpl () extends LibraryDeployer
