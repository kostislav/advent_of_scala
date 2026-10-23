package cz.judas.jan.autumn

import cz.judas.jan.advent.{InputData, histogram}

object Problem1:
  def solve(input: InputData): Int =
    input.lines
      .zipWithIndex
      .map: (line, i) =>
        val runs = line
          .foldLeft(Nil): (runs, c) =>
            runs match
              case LetterRun(`c`, count) :: tail => LetterRun(c, count + 1) :: tail
              case runs => LetterRun(c, 1) :: runs
          .histogram
          .asMap
          .map(LetterRunMeta.apply)
          .toVector
          .sortBy(_.saving)(using Ordering[Int].reverse)

        val toCompress = runs
          .take(26)
          .filter(_.saving > 0)

        val shortestLength = if toCompress.map(_.saving).sum > 2 then
          toCompress.map(_.compressedLength).sum + runs.drop(toCompress.size).map(_.uncompressedLength).sum + 2
        else
          runs.map(_.uncompressedLength).sum

        shortestLength * (i + 1)
    .sum


case class LetterRun(
  letter: Char,
  count: Int,
):
  val numChars: Int = count.toString.length + 1


case class LetterRunMeta(
  run: LetterRun,
  numOccurrences: Int,
):
  val uncompressedLength: Int = numOccurrences * run.numChars

  val compressedLength: Int = numOccurrences + run.numChars

  def saving: Int = uncompressedLength - compressedLength


@main
def doIt2(): Unit =
  val result = Problem1.solve(
    autumnInputData(1)
  )
  println(result) // TODO