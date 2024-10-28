package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData

object Day03:
  def part1(input: InputData): Long =
    val (size, counts) = input.lines
      .map(parseLine)
      .foldLeft((0, Seq.empty[Int])):
        case ((count, first), second) =>
          (count + 1, first.zipAll(second, 0, 0).map(_ + _))

    val gammaRate = binaryToInt(counts.map(count => if (count > size / 2) 1 else 0))
    val epsilonRate = (1L << counts.size) - 1 - gammaRate

    gammaRate * epsilonRate

  def part2(input: InputData): Long =
    val lines = input.lines
      .map(parseLine)
      .toSeq

    val oxygenGeneratorRating = findRating(lines, _ > _)
    val co2ScrubberRating = findRating(lines, _ <= _)

    oxygenGeneratorRating * co2ScrubberRating

  private def findRating(lines: Seq[Seq[Int]], choice: (Int, Int) => Boolean): Long =
    val numDigits = lines.head.size
    binaryToInt(
      (0 until numDigits)
        .foldLeft(lines)((candidates, digit) =>
          if (candidates.size == 1)
            candidates
          else
            val (zeroes, ones) = candidates.partition(number => number(digit) == 0)
            if (choice(zeroes.size, ones.size))
              zeroes
            else
              ones
        )
        .head
    )

  private def parseLine(line: String): Seq[Int] =
    line.toCharArray.toSeq.map(_ - '0')

  private def binaryToInt(bits: Seq[Int]): Long =
    bits.zip(bits.indices.reverse)
      .map { case (bit, magnitude) => bit << magnitude }
      .sum
