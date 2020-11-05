package se.umu.cs.rai.scopus.translator

import se.umu.cs.rai.scopus.translator.tokenizer.Translation

import scala.annotation.tailrec


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusDefinition: String = "="
  val ScopusTraitDeclaration: String = "class"
  val ScopusClassDeclaration: String = "class"
  val ScopusOpenParenthesis: String = "("
  val ScopusCloseParenthesis: String = ")"
  val ScopusSpace: String = " "
  val ScopusWith: String = ","

  val ScalaDefinition: String = "def "
  val ScalaValue: String = "val "
  val ScalaTraitDeclaration: String = "trait "
  val ScalaCaseClassDeclaration: String = "case class "
  val ScalaWith: String = " with "
  val ScalaSpace: String = " "
  val ScalaEmpty: String = ""


  def translateProgram(program: String): String =
    translateLines(program.split(NewLine).toIndexedSeq).mkString(NewLine) + NewLine

  def addSpaceToScopusLine(line: String): String = ScopusSpace + line + ScopusSpace

  def removeSpaceFromScalaLine(line: String): String = {
    val lineWithoutStartingSpace = if (line.startsWith(ScalaSpace)) {
      line.substring(1)
    } else {
      line
    }
    val lineWithoutEndingSpace = if (lineWithoutStartingSpace.endsWith(ScalaSpace)) {
      lineWithoutStartingSpace.substring(0, lineWithoutStartingSpace.length - 1)
    } else {
      lineWithoutStartingSpace
    }
    lineWithoutEndingSpace
  }

  def translateLines(lines: Seq[String]): Seq[String] = {
    CommentPreprocessor()
      .annotateLines(lines)
      .map(annotatedLine => {
        if (annotatedLine.isComment) {
          annotatedLine.line
        } else {
          removeSpaceFromScalaLine(
            translateLine(
              addSpaceToScopusLine(annotatedLine.line)
            )
          )
        }
      })
  }

  def translateLine(line: String): String = {
    Option(line)
      .flatMap(x => tryDefinition(x))
      .flatMap(x => tryStandardKeywords(x))
      .flatMap(x => tryKeywordsWithParentheses(x))
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

  def tryAbstractClassDeclaration(line: String): Some[String] = {
    if (isAbstractClassDeclaration(line)) {
      Some(replaceFirst(line, ScopusTraitDeclaration + ScopusSpace, ScalaTraitDeclaration))
    } else {
      Some(line)
    }
  }

  def isAbstractClassDeclaration(line: String): Boolean =
    line.trim.startsWith(ScopusTraitDeclaration + ScopusSpace) &&
      !line.contains(ScopusOpenParenthesis)

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

  def tryClassDeclaration(line: String): Some[String] = {
    if (isClassDeclaration(line)) {
      Some(replaceFirst(line, ScopusClassDeclaration + ScopusSpace, ScalaCaseClassDeclaration))
    } else {
      Some(line)
    }
  }

  def isClassDeclaration(line: String): Boolean =
    line.trim.startsWith(ScopusClassDeclaration + ScopusSpace) &&
      line.contains(ScopusOpenParenthesis)

  @tailrec
  final def successiveReplacements(line: String, toReplace: Seq[String], translationMap: Map[String, String]): String = {
    if (toReplace.isEmpty) {
      line
    } else {
      val keyword = toReplace.head
      val alreadyProcessedLine = replaceIfFound(line, ScopusSpace + keyword + ScopusSpace, ScalaSpace + translationMap(keyword) + ScalaSpace)
      successiveReplacements(alreadyProcessedLine, toReplace.tail, translationMap)
    }
  }

  def tryStandardKeywords(line: String): Some[String] = {
    Some(successiveReplacements(line, Translation().TranslationByKeyword.keys.toSeq, Translation().TranslationByKeyword))
  }

  def tryKeywordsWithParentheses(line: String): Some[String] = {
    val translationTable = if (line.contains(ScopusOpenParenthesis)) {
      Translation().TranslationWithParentheses
    } else {
      Translation().TranslationWithoutParentheses
    }
    Some(successiveReplacements(line, translationTable.keys.toSeq, translationTable))
  }

  def replaceIfFound(line: String, pattern: String, newText: String): String = {
    if (line.contains(pattern)) {
      replaceFirst(line, pattern, newText)
    } else {
      line
    }
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
