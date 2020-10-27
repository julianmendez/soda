package se.umu.cs.rai.scopus.translator

import java.io.FileWriter
import java.nio.file.{Files, Paths}

/**
 * This is the main entry point.
 */
case class Main() {

  val Help: String = "\n\nparameters: INPUT OUTPUT\n\nwhere\n\n  INPUT is the Scopus input file\n  OUTPUT is the Scala output file\n\n"

  val getTitleAndVersion: String = {
    val packg = this.getClass.getPackage
    val name = Option(packg.getImplementationTitle).getOrElse("")
    val version = Option(packg.getImplementationVersion).getOrElse("")
    (name + " " + version).trim
  }

  def run(args: Array[String]): Unit = {
    if (args.length == 2) {
      val input = readFile(args(0))
      val output = MicroTranslator().translateProgram(input)
      val writer = new FileWriter(args(1))
      writer.write(output)
      writer.flush()
    }
    else println(getTitleAndVersion + Help)
  }

  def readFile(fileName: String): String = {
    new String(Files.readAllBytes(Paths.get(fileName)))
  }

}

object Main {
  def main(args: Array[String]): Unit = new Main().run(args)
}

