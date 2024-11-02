package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData

import scala.annotation.tailrec

object Day03:
  def part1(input: InputData): Long =
    val lines = input.lines.toSeq
    val counts = lines
      .map(parseLine)
      .reduce { (first, second) => first.zip(second).map(_ + _) }

    val gammaRate = binaryToInt(counts.map(count => if (count > lines.size / 2) 1 else 0))
    val epsilonRate = (1L << counts.size) - 1 - gammaRate

    gammaRate * epsilonRate

  def part2(input: InputData): Long =
    val lines = input.lines
      .map(parseLine)
      .toSeq

    val oxygenGeneratorRating = findRating(lines, _ > _)
    val co2ScrubberRating = findRating(lines, _ <= _)

    oxygenGeneratorRating * co2ScrubberRating

  private def findRating(candidates: Seq[Seq[Int]], choice: (Int, Int) => Boolean): Long =
    findRating(0, candidates, choice)

  @tailrec
  private def findRating(digit: Int, candidates: Seq[Seq[Int]], choice: (Int, Int) => Boolean): Long =
    if (candidates.size == 1)
      binaryToInt(candidates.head)
    else
      val (zeroes, ones) = candidates.partition(number => number(digit) == 0)
      findRating(
        digit + 1,
        if (choice(zeroes.size, ones.size)) zeroes else ones,
        choice,
      )

  private def parseLine(line: String): Seq[Int] =
    line.toCharArray.toSeq.map(_ - '0')

  private def binaryToInt(bits: Seq[Int]): Long =
    bits.zip(bits.indices.reverse)
      .map { case (bit, magnitude) => bit << magnitude }
      .sum
