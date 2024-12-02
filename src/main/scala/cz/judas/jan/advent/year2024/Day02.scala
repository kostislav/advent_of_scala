package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

object Day02:
  def part1(input: InputData): Int =
    input.lines
      .count(line =>
        val report = line.split(" ").map(x => x.toInt).toList
        isSafe(report)
      )

  def part2(input: InputData): Int =
    input.lines
      .count(line =>
        val report = line.split(" ").map(x => x.toInt).toList
        if isSafe(report) then
          true
        else
          (0 to report.size).exists(i =>
            val newReport = report.zipWithIndex.filter(_._2 != i).map(_._1)
            isSafe(newReport)
          )
      )

  private def isSafe(report: List[Int]): Boolean =
    val differences = report.sliding(2).map(window => window(1) - window.head).toList
    differences.forall(diff => diff >= 1 && diff <= 3) || differences.forall(diff => diff <= -1 && diff >= -3)
