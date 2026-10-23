package cz.judas.jan.autumn

import cz.judas.jan.advent.{InputData, maybeIndexOf}

object Problem0:
  def solve(input: InputData): Int =
    val text :: words = input.lines.toList: @unchecked

    val noSpaceText = text.replace(" ", "")
    val wordIndexes = text
      .scanLeft((0, false)):
        case ((index, isLetter), c) =>
          if c == ' ' then (index + 1, false) else (index, true)
      .filter(_._2)
      .map(_._1)

    words
      .map: word =>
        noSpaceText.maybeIndexOf(word)
          .map(wordIndexes)
          .getOrElse(-1)
      .sum


@main
def doIt(): Unit =
  val result = Problem0.solve(
    autumnInputData(0)
  )
  println(result) // TODO