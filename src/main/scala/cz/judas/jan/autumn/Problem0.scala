package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData

import java.nio.file.{Files, Paths}
import language.experimental.relaxedLambdaSyntax

object Problem0:
  def solve(input: InputData): Int =
    val text :: words = input.lines.toList: @unchecked

    val indexedWords = text.split(' ')
      .zipWithIndex
      .flatMap: (word, index) =>
        word.map: c => (c, index)

    words
      .map: word =>
        indexedWords.sliding(word.length)
          .find: subtext =>
            subtext.map(_._1).mkString("") == word
          .map(_(0)._2)
          .getOrElse(-1)
      .sum


@main
def doIt(): Unit =
  val result = Problem0.solve(
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