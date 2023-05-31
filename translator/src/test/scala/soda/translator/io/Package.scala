package soda.translator.io

/*
 * This package contains tests for the directory scanner.
 */

trait Package

case class DirectoryScannerSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   java.io.File

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val start = "translator/src/test/resources/soda/example"

  lazy val single_start_file = new File (start , "Example.soda")

  test ("scan a file that is not a directory") (
    check (
      obtained = DirectoryScanner_ () .get_all_files (single_start_file) .toSet
    ) (
      expected = Seq (single_start_file) .toSet
    )
  )

  test ("simple scan of all files") (
    check (
      obtained = DirectoryScanner_ () .get_all_files ( new File (start) ) .toSet
    ) (
      expected = Seq (
        "otherexample",
        "Example.md",
        "Example.scala",
        "Example.soda",
        "Package.scala",
        "Package.soda",
        "otherexample/OtherExample.scala",
        "otherexample/OtherExample.soda",
        "otherexample/OtherExample.txt",
        "otherexample/Package.scala",
        "otherexample/Package.soda"
      ) .map ( x => new File (start , x) ) .toSet
    )
  )

}

