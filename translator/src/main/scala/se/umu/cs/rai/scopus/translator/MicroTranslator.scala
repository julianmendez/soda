package se.umu.cs.rai.scopus.translator


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusDefinition: String = " " + "=" + " "
  val ScopusLambdaExpression: String = " " + "-" + ">"
  val ScopusTraitDeclaration: String = "class "
  val ScopusClassDeclaration: String = "class "
  val ScopusOpenParenthesis: String = "("
  val ScopusCloseParenthesis: String = ")"
  val ScopusIf: String = "if "
  val ScopusThen: String = " then "
  val ScopusElse: String = "else "
  val ScopusElseIf: String = "else if "
  val ScopusTypeDeclaration: String = " " + ":" + " "
  val ScopusSpace: String = " "
  val ScopusWith: String = ","

  val ScalaDefinition: String = "def "
  val ScalaValue: String = "val "
  val ScalaLambdaExpression: String = " =>"
  val ScalaTraitDeclaration: String = "trait "
  val ScalaCaseClassDeclaration: String = "case class "
  val ScalaExtends: String = " extends "
  val ScalaWith: String = " with "
  val ScalaOpenBrace: String = " {"
  val ScalaCloseBrace: String = "}"
  val ScalaParentheses: String = " () "
  val ScalaIf: String = "if ("
  val ScalaThen: String = ") "
  val ScalaElse: String = "else "
  val ScalaElseIf: String = "else if "
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
      .flatMap(x => tryThen(x))
      .flatMap(x => tryElseIf(x))
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
    if (line.trim.startsWith(ScopusTraitDeclaration.trim) && !line.contains(ScopusOpenParenthesis)) {
      Some(replaceFirst(line, ScopusTraitDeclaration.trim, ScalaTraitDeclaration))
    } else {
      Some(line)
    }
  }

  def tryClassDeclaration(line: String): Some[String] = {
    if (line.trim.startsWith(ScopusClassDeclaration) && line.contains(ScopusOpenParenthesis)) {
      Some(replaceFirst(line, ScopusClassDeclaration.trim, ScalaCaseClassDeclaration))
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

  def replaceIfFound(line: String, pattern: String, newText: String): String = {
    if (line.contains(pattern)) {
      replaceFirst(line, pattern, newText)
    } else {
      line
    }
  }

  def tryElse(line: String): Some[String] = {
    Some(replaceIfFound(line, ScopusElseIf, ScalaElseIf))
  }

  def tryElseIf(line: String): Some[String] = {
    Some(replaceIfFound(line, ScopusElse, ScalaElse))
  }

  def tryThen(line: String): Some[String] = {
    Some(replaceIfFound(line, ScopusThen, ScalaThen))
  }

  def tryIf(line: String): Some[String] = {
    Some(replaceIfFound(line, ScopusIf, ScalaIf))
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


  def addIfNonEmpty(textToPrepend: String, line: String): String = {
    if (line.trim.isEmpty) {
      line
    } else {
      textToPrepend + line
    }
  }

}
