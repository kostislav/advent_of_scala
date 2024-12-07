package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

object Day07:
  def part1(input: InputData): Long =
    solve(input, concatenate = false)

  def part2(input: InputData): Long =
    solve(input, concatenate = true)

  private def solve(input: InputData, concatenate: Boolean): Long =
    input.lines
      .flatMap: line =>
        val Array(testValueString, numberString) = line.split(": ")
        val testValue = testValueString.toLong
        val numbers = numberString.split(" ").map(_.toLong)
        Some(testValue).filter(bleh(_, 0, numbers, 0, concatenate))
      .sum

  private def bleh(targetResult: Long, partialResult: Long, numbers: IndexedSeq[Long], position: Int, concatenate: Boolean): Boolean =
    if partialResult > targetResult then
      false
    else if position == numbers.size then
      partialResult == targetResult
    else
      bleh(targetResult, partialResult + numbers(position), numbers, position + 1, concatenate)
        || bleh(targetResult, partialResult * numbers(position), numbers, position + 1, concatenate)
        || (concatenate && bleh(targetResult, s"${partialResult}${numbers(position)}".toLong, numbers, position + 1, concatenate))

