package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData

object Day03:
  def part1(input: InputData): Long =
    val (size, counts) = input.lines
      .map(_.toCharArray.toSeq.map(_ - '0'))
      .foldLeft((0, Seq.empty[Int])):
        case ((count, first), second) =>
          (count + 1, first.zipAll(second, 0, 0).map(_ + _))

    val gammaRate = counts.zip(counts.indices.reverse.map(1L << _))
      .map{ case (count, magnitude) => if (count > size / 2) magnitude else 0 }
      .sum
    val epsilonRate = (1L << counts.size) - 1 - gammaRate

    gammaRate * epsilonRate
