package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusOpeningParenthesis: String = "("
  val ScopusClosingParenthesis: String = ")"
  val ScopusSpace: String = " "
  val ScalaSpace: String = " "


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

  def tokenize(line: String): Seq[Token] =
    Tokenizer().tokenize(line)

  def joinTokens(tokens: Seq[Token]): String =
    tokens
      .map(token => token.text)
      .mkString("")

  def translateLines(lines: Seq[String]): Seq[String] =
    CommentPreprocessor()
      .annotateLines(lines)
      .map(annotatedLine => {
        if (annotatedLine.isComment) {
          annotatedLine.line
        } else {
          val line = annotatedLine.line
          val lineWithSpace = addSpaceToScopusLine(line)
          val tokenizedLine = tokenize(lineWithSpace)
          val translatedLine = translateLine(tokenizedLine)
          val jointLine = joinTokens(translatedLine)
          val finalLine = removeSpaceFromScalaLine(jointLine)
          finalLine
        }
      })

  def translateLine(tokens: Seq[Token]): Seq[Token] =
    tokens.map(
      token => if (token.parserState == ParserState().Plain) {
        val currentLine = token.text
        val newText = Option(currentLine)
          .flatMap(line => replaceAtBeginning(line, token.index, Translation().SynonymAtBeginning))
          .flatMap(line => replace(line, Translation().Synonym, onlyBeginning = false))
          .flatMap(line => tryDefinition(line))
          .flatMap(line => replaceAtBeginning(line, token.index, getTranslationTableAtBeginning(line)))
          .flatMap(line => replace(line, Translation().Translation, onlyBeginning = false))
          .getOrElse(currentLine)
        Token(newText, token.parserState, token.index)
      } else {
        token
      })

  def getTranslationTableAtBeginning(line: String): Seq[(String, String)] =
    if (line.contains(ScopusOpeningParenthesis)) {
      Translation().TranslationAtBeginningWithParen
    } else {
      Translation().TranslationAtBeginningWithoutParen
    }

  /**
   * A line containing the definition sign will be classified as a definition.
   * The definitions need to be translated to either 'val' or 'def'.
   *
   * 'val' is for value definition.
   * It is detected in three cases.
   * Case 1: The line does not have a closing parenthesis, e.g. `a = 1`
   * Case 2: The first non-blank character of a line is an open parenthesis, e.g. `(x, y) = (0, 1)`
   * Case 3: The first closing parenthesis is after the definition sign, e.g. `x = f(y)`
   *
   * 'def' is for function definition.
   * If it does not fit in any of the 'val' cases.
   *
   * @param line line
   * @return maybe a translated line
   */
  def tryDefinition(line: String): Some[String] = {
    val maybePosition = findDefinition(line)
    if (maybePosition.nonEmpty) {
      val positionOfDefinitionSign = maybePosition.get
      val positionOfFirstClosingParenthesis = line.indexOf(ScopusClosingParenthesis)

      val case1 = positionOfFirstClosingParenthesis == -1
      val case2 = line.trim.startsWith(ScopusOpeningParenthesis)
      val case3 = positionOfFirstClosingParenthesis > positionOfDefinitionSign

      if (case1 || case2 || case3) {
        Some(addAfterSpaces(Translation().ScalaValue + ScalaSpace, line))
      } else {
        Some(addAfterSpaces(Translation().ScalaDefinition + ScalaSpace, line))
      }
    } else {
      Some(line)
    }
  }


  /**
   * A line is a definition when its main operator is "=" (the equals sign), which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */
  def findDefinition(line: String): Option[Int] = {
    val position = if (line.endsWith(ScopusSpace + Translation().ScopusDefinition)) {
      line.length - Translation().ScopusDefinition.length
    } else {
      line.indexOf(ScopusSpace + Translation().ScopusDefinition + ScopusSpace)
    }
    if (position == -1) {
      None
    } else {
      Some(position)
    }
  }

  def replaceAll(line: String, pattern: String, replacement: String): String =
    replaceAllRec(line, pattern, replacement, Seq())

  @tailrec
  final def replaceAllRec(line: String, pattern: String, replacement: String, replacedTextRev: Seq[String]): String = {
    val pos = line.indexOf(pattern)
    if (pos == -1) {
      replacedTextRev.prepended(line)
        .reverse
        .mkString("")
    } else {
      val newReplacedTextRev = replacedTextRev.prepended(line.substring(0, pos) + replacement)
      val newLine = line.substring(pos + pattern.length)
      replaceAllRec(newLine, pattern, replacement, newReplacedTextRev)
    }
  }

  def replaceAtBeginning(line: String, index: Int, translationTable: Seq[(String, String)]): Some[String] =
    if (index == 0) {
      replace(line, translationTable, onlyBeginning = true)
    } else {
      Some(line)
    }


  def replace(line: String, translationTable: Seq[(String, String)], onlyBeginning: Boolean): Some[String] = {
    val keys = translationTable.map(pair => pair._1)
    Some(replaceRec(line, keys, translationTable.toMap, onlyBeginning))
  }

  @tailrec
  final def replaceRec(line: String, toReplace: Seq[String], translationMap: Map[String, String], onlyBeginning: Boolean): String =
    if (toReplace.isEmpty) {
      line
    } else {
      val reservedWord = toReplace.head
      val alreadyProcessedLine =
        replaceIfFound(line, ScopusSpace + reservedWord + ScopusSpace, ScalaSpace + translationMap(reservedWord) + ScalaSpace, onlyBeginning)
      replaceRec(alreadyProcessedLine, toReplace.tail, translationMap, onlyBeginning)
    }

  def replaceIfFound(line: String, pattern: String, newText: String, onlyBeginning: Boolean): String =
    if ((onlyBeginning && line.trim.startsWith(pattern.trim)) ||
      (!onlyBeginning && line.contains(pattern))) {
      replaceAll(line, pattern, newText)
    } else {
      line
    }

  def addAfterSpaces(textToPrepend: String, line: String): String = {
    val prefixLength = line.takeWhile(ch => ch.isSpaceChar).length
    line.substring(0, prefixLength) + textToPrepend + line.substring(prefixLength)
  }

  def addIfNonEmpty(textToPrepend: String, line: String): String =
    if (line.trim.isEmpty) {
      line
    } else {
      textToPrepend + line
    }

}
