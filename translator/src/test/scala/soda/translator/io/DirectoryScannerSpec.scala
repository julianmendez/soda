package soda.translator.io

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

case class DirectoryScannerSpec () extends AnyFunSuite {

  lazy val start = "translator/src/test/resources/soda/example"

  test ("scan a file that is not a directory") {
    lazy val scanner = DirectoryScanner ()
    lazy val start_file = new File (start, "Example.soda")
    lazy val expected = Seq (start_file ) .toSet
    lazy val obtained = scanner.get_all_files (start_file ) .toSet
    assert (obtained == expected )
  }

  test ("simple scan of all files") {
    lazy val scanner = DirectoryScanner ()

    lazy val expected = Seq ("otherexample", "Example.md", "Example.scala", "Example.soda", "otherexample/OtherExample.scala", "otherexample/OtherExample.soda", "otherexample/OtherExample.txt"
    ) .map (x => new File (start, x )  ) .toSet
    lazy val start_file = new File (start )
    lazy val obtained = scanner.get_all_files (start_file ) .toSet
    assert (obtained == expected )
  }

}
