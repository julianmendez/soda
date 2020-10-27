package se.umu.cs.rai.scopus.translator


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusDefinition: String = " " + "=" + " "
  val ScopusLambdaExpression: String = " " + "-" + ">"
  val ScopusClassOrTraitDeclaration: String = " " + ":" + ":" + " "
  val ScopusOpenParenthesis: String = " " + "("
  val ScopusCloseParenthesis: String = ")"
  val ScopusIf: String = " " + "?"
  val ScopusElse: String = " " + "|" + " "
  val ScopusTypeDeclaration: String = " " + ":" + " "
  val ScopusSpace: String = " "
  val ScopusWith: String = ","
  val ScopusMain: String = "@" + "main"
  val ScopusNew: String = "@" + "new "

  val ScalaPackage: String = "package "
  val ScalaImport: String = "import "
  val ScalaDefinition: String = "val "
  val ScalaLambdaExpression: String = " =>"
  val ScalaTraitDeclaration: String = "trait "
  val ScalaClassDeclaration: String = "case class "
  val ScalaExtends: String = " extends "
  val ScalaWith: String = " with "
  val ScalaOpenBrace: String = " {"
  val ScalaCloseBrace: String = "}"
  val ScalaParentheses: String = " () "
  val ScalaIf: String = "if "
  val ScalaElse: String = "else "
  val ScalaTypeDeclaration: String = " : "
  val ScalaSpace: String = " "
  val ScalaEmpty: String = ""
  val ScalaMain: String = "object Main {\n  def main(args: Array[String]): Unit = new Main().run(args)\n}\n"
  val ScalaNew: String = "new "


  def translateProgram(program: String): String = {
    translateLines(program.split(NewLine).toIndexedSeq).mkString(NewLine) + NewLine
  }


  def translateLines(lines: Seq[String]): Seq[String] = {
    tryPackageAndImport(lines)
      .map(x => translateLine(x))
  }


  def tryPackageAndImport(lines: Seq[String]): Seq[String] = {
    if (lines.isEmpty || lines.head.trim.isEmpty || lines.head.contains(ScopusClassOrTraitDeclaration)) {
      lines
    } else {
      addAfterSpaces(ScalaPackage, lines.head) +: tryImport(lines.tail)
    }
  }


  def tryImport(lines: Seq[String]): Seq[String] = {
    val imports = lines.takeWhile(line =>
      !line.contains(ScopusClassOrTraitDeclaration) && (line.trim.isEmpty || line.trim.charAt(0).isLetter))
    val translatedImports = imports.map(line => addIfNonEmpty(ScalaImport, line))
    val remainingProgram = lines.slice(imports.length, lines.length)
    Seq() ++ translatedImports ++ remainingProgram
  }

  def addIfNonEmpty(textToPrepend: String, line: String): String = {
    if (line.trim.isEmpty) {
      line
    } else {
      textToPrepend + line
    }
  }

  def translateLine(line: String): String = {
    Option(line)
      .flatMap(x => tryOpenParenthesis(x))
      .flatMap(x => tryCloseParenthesis(x))
      .flatMap(x => tryDefinition(x))
      .flatMap(x => tryLambdaArrow(x))
      .flatMap(x => tryTraitDeclaration(x))
      .flatMap(x => tryClassDeclaration(x))
      .flatMap(x => tryIf(x))
      .flatMap(x => tryElse(x))
      .flatMap(x => tryMain(x))
      .flatMap(x => tryNew(x))
      .getOrElse(line)
  }

  def tryOpenParenthesis(line: String): Some[String] = {
    if (line.trim.endsWith(ScopusOpenParenthesis)) {
      val pos = line.lastIndexOf(ScopusOpenParenthesis)
      val newLine = line.substring(0, pos) + ScalaOpenBrace + line.substring(pos + ScopusOpenParenthesis.length)
      Some(newLine)
    } else {
      Some(line)
    }
  }

  def tryCloseParenthesis(line: String): Some[String] = {
    if (line.trim.startsWith(ScopusCloseParenthesis.trim)) {
      Some(replaceFirst(line, ScopusCloseParenthesis, ScalaCloseBrace))
    } else {
      Some(line)
    }
  }

  def tryDefinition(line: String): Some[String] = {
    if (line.contains(ScopusDefinition)) {
      Some(addAfterSpaces(ScalaDefinition, line))
    } else {
      Some(line)
    }
  }

  def tryTraitDeclaration(line: String): Some[String] = {
    if (line.trim.startsWith(ScopusClassOrTraitDeclaration.trim)) {
      Some(replaceFirst(line, ScopusClassOrTraitDeclaration.trim, ScalaTraitDeclaration))
    } else {
      Some(line)
    }
  }

  def tryClassDeclaration(line: String): Some[String] = {
    if (line.contains(ScopusClassOrTraitDeclaration) && !line.trim.startsWith(ScopusClassOrTraitDeclaration.trim)) {
      val pos = line.indexOf(ScopusClassOrTraitDeclaration)
      val firstPart = line.substring(0, pos)
      val remainingPart = line.substring(pos + ScopusClassOrTraitDeclaration.length).trim
      val newRemainingPart = {
        if (remainingPart.nonEmpty && remainingPart.charAt(0).isLetterOrDigit) {
          ScalaExtends + remainingPart.replaceAll(ScopusWith, ScalaWith)
        }
        else {
          remainingPart
        }
      }
      Some(ScalaClassDeclaration + firstPart + ScalaSpace + newRemainingPart)
    } else {
      Some(line)
    }
  }

  def tryLambdaArrow(line: String): Some[String] = {
    if (line.contains(ScopusLambdaExpression)) {
      Some(line.replace(ScopusLambdaExpression, ScalaLambdaExpression))
    } else {
      Some(line)
    }
  }

  def tryIf(line: String): Some[String] = {
    if (line.contains(ScopusIf)) {
      val updatedLine = {
        if (line.trim.startsWith(ScopusElse.trim)) {
          addAfterSpaces(ScalaElse + ScalaIf,
            replaceFirst(replaceFirst(line, ScopusElse, ScalaSpace), ScopusIf, ScalaEmpty))
        }
        else {
          addAfterSpaces(ScalaIf, replaceFirst(line, ScopusIf, ScalaEmpty))
        }
      }
      Some(updatedLine)
    } else {
      Some(line)
    }
  }

  def addAfterSpaces(textToPrepend: String, line: String): String = {
    val prefixLength = line.takeWhile(ch => ch.isSpaceChar).length
    line.substring(0, prefixLength) + textToPrepend + line.substring(prefixLength)
  }

  def replaceFirst(line: String, pattern: String, replacement: String): String = {
    val pos = line.indexOf(pattern)
    val result = {
      if (pos == -1) {
        line
      }
      else {
        line.substring(0, pos) + replacement + line.substring(pos + pattern.length)
      }
    }
    result
  }

  def tryElse(line: String): Some[String] = {
    if (line.contains(ScopusElse)) {
      Some(replaceFirst(line, ScopusElse, ScalaSpace + ScalaElse))
    } else {
      Some(line)
    }
  }

  def tryMain(line: String): Some[String] = {
    if (line.trim.startsWith(ScopusMain)) {
      Some(replaceFirst(line, ScopusMain, ScalaMain))
    } else {
      Some(line)
    }
  }

  def tryNew(line: String): Some[String] = {
    if (line.contains(ScopusNew)) {
      Some(line.replace(ScopusNew, ScalaNew))
    } else {
      Some(line)
    }
  }

}
