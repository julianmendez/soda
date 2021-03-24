package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

case class ScannerSpec (  ) extends AnyFunSuite {

  lazy val start = "translator/src/test/resources/scopus/example"

  test ("scan a file that is not a directory") {
    lazy val scanner = DirectoryScanner (  )
    lazy val start_file = new File ( start , "Example.scopus")
    lazy val expected = Seq ( start_file ) .toSet
    lazy val obtained = scanner.get_all_files ( start_file ) .toSet
    assert ( obtained == expected )
  }

  test ("simple scan of all files") {
    lazy val scanner = DirectoryScanner (  )

    lazy val expected = Seq (
      "otherexample",      "Example.md",      "Example.scala",      "Example.scopus",      "otherexample/OtherExample.scala",      "otherexample/OtherExample.scopus",      "otherexample/OtherExample.txt"
    ) .map ( x => new File ( start , x )  ) .toSet
    lazy val start_file = new File ( start )
    lazy val obtained = scanner.get_all_files ( start_file ) .toSet
    assert ( obtained == expected )
  }

  test ("simple get all Scopus files") {
    lazy val scanner = DirectoryScanner (  )

    lazy val expected = Seq (
      "Example.scopus",      "otherexample/OtherExample.scopus"
    ) .map ( x => new File ( start , x )  ) .toSet
    lazy val start_file = new File ( start )
    lazy val obtained = scanner.get_scopus_files ( start_file ) .toSet
    assert ( obtained == expected )
  }

}
