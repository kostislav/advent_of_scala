package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, filterByIndex, separatedBy}

object Day02:
  def part1(input: InputData): Int =
    input.linesAs[Seq[Int] @separatedBy(" ")]
      .count(isSafe)

  def part2(input: InputData): Int =
    input.linesAs[Seq[Int] @separatedBy(" ")]
      .count: report =>
        isSafe(report) || report.indices.exists(i => isSafe(report.filterByIndex(_ != i)))

  private def isSafe(report: Seq[Int]): Boolean =
    val differences = report.sliding(2).map { case Seq(first, second) => first - second }.toSeq
    differences.forall(isSafe) || differences.forall(diff => isSafe(-diff))

  private def isSafe(diff: Int): Boolean =
    diff >= 1 && diff <= 3
