package cz.judas.jan.autumn

import cz.judas.jan.advent.{InputData, maybeIndexOf}

import java.nio.file.{Files, Paths}
import language.experimental.relaxedLambdaSyntax

def problem0(input: InputData): Int =
  val text :: words = input.lines.toList: @unchecked

  val noSpaceText = text.replace(" ", "")
  val wordIndexes = text
    .scanLeft((0, false)): case ((index, isLetter), c) =>
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
  val result = problem0(
//    InputData.fromString(
//      """
//      the quick brown fox jumps over the lazy dog
//      quic
//      sover
//      dog
//      deer
//      """
//    )
    InputData.fromString(Files.readString(Paths.get("input/autumn/0")))
  )
  println(result) // TODO