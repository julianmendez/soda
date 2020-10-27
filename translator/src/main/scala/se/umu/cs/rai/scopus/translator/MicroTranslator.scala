package se.umu.cs.rai.scopus.translator


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusDefinition: String = " " + "=" + " "
  val ScopusLambdaExpression: String = " " + "-" + ">"
  val ScopusTraitDeclaration: String = "trait"
  val ScopusClassDeclaration: String = "class"
  val ScopusOpenParenthesis: String = " " + "("
  val ScopusCloseParenthesis: String = ")"
  val ScopusIf: String = " " + "?"
  val ScopusElse: String = " " + "|" + " "
  val ScopusTypeDeclaration: String = " " + ":" + " "
  val ScopusSpace: String = " "
  val ScopusWith: String = ","

  val ScalaDefinition: String = "def "
  val ScalaValue: String = "val "
  val ScalaLambdaExpression: String = " =>"
  val ScalaTraitDeclaration: String = "trait "
  val ScalaCaseDeclaration: String = "case "
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


  def translateProgram(program: String): String = {
    translateLines(program.split(NewLine).toIndexedSeq).mkString(NewLine) + NewLine
  }


  def translateLines(lines: Seq[String]): Seq[String] = {
    lines
      .map(x => translateLine(x))
  }

  def translateLine(line: String): String = {
    Option(line)
      .flatMap(x => tryDefinition(x))
      .flatMap(x => tryLambdaArrow(x))
      .flatMap(x => tryTraitDeclaration(x))
      .flatMap(x => tryClassDeclaration(x))
      .flatMap(x => tryIf(x))
      .flatMap(x => tryElse(x))
      .getOrElse(line)
  }

  def tryDefinition(line: String): Some[String] = {
    val index = line.indexOf(ScopusDefinition)
    val definitionFound = index != -1
    if (definitionFound || line.endsWith(ScopusDefinition.trim)) {
      val indexOfParenthesis = line.indexOf(ScopusCloseParenthesis)
      if (indexOfParenthesis == -1) {
        Some(addAfterSpaces(ScalaValue, line))
      } else {
        Some(addAfterSpaces(ScalaDefinition, line))
      }
    } else {
      Some(line)
    }
  }

  def tryTraitDeclaration(line: String): Some[String] = {
    if (line.trim.startsWith(ScopusTraitDeclaration.trim)) {
      Some(replaceFirst(line, ScopusTraitDeclaration.trim, ScalaTraitDeclaration))
    } else {
      Some(line)
    }
  }

  def tryClassDeclaration(line: String): Some[String] = {
    if (line.trim.startsWith(ScopusClassDeclaration)) {
      Some(ScalaCaseDeclaration + line)
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

  def addAfterSpaces(textToPrepend: String, line: String): String = {
    val prefixLength = line.takeWhile(ch => ch.isSpaceChar).length
    line.substring(0, prefixLength) + textToPrepend + line.substring(prefixLength)
  }

  def tryElse(line: String): Some[String] = {
    if (line.contains(ScopusElse)) {
      Some(replaceFirst(line, ScopusElse, ScalaSpace + ScalaElse))
    } else {
      Some(line)
    }
  }

  def addIfNonEmpty(textToPrepend: String, line: String): String = {
    if (line.trim.isEmpty) {
      line
    } else {
      textToPrepend + line
    }
  }

}
