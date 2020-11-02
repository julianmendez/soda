package se.umu.cs.rai.scopus.translator


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusDefinition: String = "="
  val ScopusLambdaExpression: String = " " + "-" + ">"
  val ScopusTraitDeclaration: String = "class"
  val ScopusClassDeclaration: String = "class"
  val ScopusOpenParenthesis: String = "("
  val ScopusCloseParenthesis: String = ")"
  val ScopusIf: String = "if "
  val ScopusThen: String = "then"
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


  def translateProgram(program: String): String =
    translateLines(program.split(NewLine).toIndexedSeq).mkString(NewLine) + NewLine

  def translateLines(lines: Seq[String]): Seq[String] =
    lines.map(x => translateLine(x))

  def translateLine(line: String): String = {
    Option(line)
      .flatMap(x => tryDefinition(x))
      .flatMap(x => tryLambdaArrow(x))
      .flatMap(x => tryAbstractClassDeclaration(x))
      .flatMap(x => tryClassDeclaration(x))
      .flatMap(x => tryThen(x))
      .flatMap(x => tryElseIf(x))
      .flatMap(x => tryIf(x))
      .flatMap(x => tryElse(x))
      .getOrElse(line)
  }

  /**
   * A line is a definition when its main operator is "=" (the equals sign).
   *
   * @param line line
   * @return if it is a definition
   */
  def isDefinition(line: String): Boolean =
    (line.indexOf(ScopusSpace + ScopusDefinition + ScopusSpace) != -1) ||
      line.endsWith(ScopusSpace + ScopusDefinition)

  /**
   * There are two types of definitions: 'val' and 'def'.
   * 'val' is for values.
   * It is detected in two cases.
   * One case is when before the equals sign there no parentheses, which is a typical constant.
   * Another case is when there is a closing parenthesis next to it but the line starts with an opening parenthesis, which is a tuple definition.
   *
   * 'def' is for function definition.
   * It is detected when there is a closing parenthesis at the left of the equals sign, but the line starts with an identifier.
   *
   * @param line line
   * @return maybe a translated line
   */
  def tryDefinition(line: String): Some[String] = {
    if (isDefinition(line)) {
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

  def isAbstractClassDeclaration(line: String): Boolean =
    line.trim.startsWith(ScopusTraitDeclaration + ScopusSpace) &&
      !line.contains(ScopusOpenParenthesis)

  def tryAbstractClassDeclaration(line: String): Some[String] = {
    if (isAbstractClassDeclaration(line)) {
      Some(replaceFirst(line, ScopusTraitDeclaration + ScopusSpace, ScalaTraitDeclaration))
    } else {
      Some(line)
    }
  }

  def isClassDeclaration(line: String): Boolean =
    line.trim.startsWith(ScopusClassDeclaration + ScopusSpace) &&
      line.contains(ScopusOpenParenthesis)

  def tryClassDeclaration(line: String): Some[String] = {
    if (isClassDeclaration(line)) {
      Some(replaceFirst(line, ScopusClassDeclaration + ScopusSpace, ScalaCaseClassDeclaration))
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

  def tryElseIf(line: String): Some[String] =
    Some(replaceIfFound(line, ScopusElseIf, ScalaElseIf))

  def tryElse(line: String): Some[String] =
    Some(replaceIfFound(line, ScopusElse, ScalaElse))

  def tryThen(line: String): Some[String] =
    Some(replaceIfFound(line, ScopusSpace + ScopusThen + ScopusSpace, ScalaThen))

  def tryIf(line: String): Some[String] =
    Some(replaceIfFound(line, ScopusIf, ScalaIf))

  def replaceFirst(line: String, pattern: String, replacement: String): String = {
    val pos = line.indexOf(pattern)
    val result = {
      if (pos == -1) {
        line
      } else {
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
