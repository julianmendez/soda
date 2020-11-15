package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusOpenParenthesis: String = "("
  val ScopusCloseParenthesis: String = ")"
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

  def translateLines(lines: Seq[String]): Seq[String] = {
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
  }

  def translateLine(tokens: Seq[Token]): Seq[Token] = {
    tokens.map(
      token => if (token.parserState == ParserState().Plain) {
        val line = token.text
        val newText = Option(line)
          .flatMap(x => tryDefinition(x))
          .flatMap(x => tryStandardKeywords(x))
          .flatMap(x => tryKeywordsWithParentheses(x))
          .getOrElse(line)
        Token(newText, token.parserState)
      } else {
        token
      })
  }

  /**
   * A line is a definition when its main operator is "=" (the equals sign).
   *
   * @param line line
   * @return if it is a definition
   */
  def isDefinition(line: String): Boolean =
    (line.indexOf(ScopusSpace + Translation().ScopusDefinition + ScopusSpace) != -1) ||
      line.endsWith(ScopusSpace + Translation().ScopusDefinition)

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
        Some(addAfterSpaces(Translation().ScalaValue + ScalaSpace, line))
      } else {
        Some(addAfterSpaces(Translation().ScalaDefinition + ScalaSpace, line))
      }
    } else {
      Some(line)
    }
  }


  def replaceAll(line: String, pattern: String, replacement: String): String =
    replaceAllRec(line, pattern, replacement, Seq())

  @tailrec
  final def replaceAllRec(line: String, pattern: String, replacement: String, replacedTextRev: Seq[String]): String = {
    val pos = line.indexOf(pattern)
    if (pos == -1) {
      (line +: replacedTextRev)
        .reverse
        .mkString("")
    } else {
      val newReplacedTextRev = (line.substring(0, pos) + replacement) +: replacedTextRev
      val newLine = line.substring(pos + pattern.length)
      replaceAllRec(newLine, pattern, replacement, newReplacedTextRev)
    }
  }


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
    val keys = Translation().TranslationByKeyword.map(pair => pair._1)
    Some(successiveReplacements(line, keys, Translation().TranslationByKeyword.toMap))
  }

  def tryKeywordsWithParentheses(line: String): Some[String] = {
    val translationTable = if (line.contains(ScopusOpenParenthesis)) {
      Translation().TranslationWithParentheses
    } else {
      Translation().TranslationWithoutParentheses
    }
    val keys = translationTable.map(pair => pair._1)
    Some(successiveReplacements(line, keys, translationTable.toMap))
  }

  def replaceIfFound(line: String, pattern: String, newText: String): String = {
    if (line.contains(pattern)) {
      replaceAll(line, pattern, newText)
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
