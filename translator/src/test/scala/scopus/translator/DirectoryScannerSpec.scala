package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

case class ScannerSpec() extends AnyFunSuite {

  val start = "translator/src/test/resources/scopus/example"

  test("scan a file that is not a directory") {
    val scanner = DirectoryScanner()
    val start_file = new File(start, "Example.scopus")
    val expected = Seq(start_file).toSet
    val obtained = scanner.get_all_files(start_file).toSet
    assert(obtained == expected)
  }

  test("simple scan of all files") {
    val scanner = DirectoryScanner()

    val expected = Seq(
      "otherexample",      "Example.md",      "Example.scala",      "Example.scopus",      "otherexample/OtherExample.scala",      "otherexample/OtherExample.scopus",      "otherexample/OtherExample.txt"
    ).map(x => new File(start, x)).toSet
    val start_file = new File(start)
    val obtained = scanner.get_all_files(start_file).toSet
    assert(obtained == expected)
  }

  test("simple get all Scopus files") {
    val scanner = DirectoryScanner()

    val expected = Seq(
      "Example.scopus",      "otherexample/OtherExample.scopus"
    ).map(x => new File(start, x)).toSet
    val start_file = new File(start)
    val obtained = scanner.get_scopus_files(start_file).toSet
    assert(obtained == expected)
  }

}
