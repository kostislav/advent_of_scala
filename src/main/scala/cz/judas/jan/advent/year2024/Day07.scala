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

  private def bleh(targetResult: Long, partialResult: Long, numbers: IndexedSeq[Long], position: Int, useConcatenate: Boolean): Boolean =
    if partialResult > targetResult then
      false
    else if position == numbers.size then
      partialResult == targetResult
    else
      bleh(targetResult, partialResult + numbers(position), numbers, position + 1, useConcatenate)
        || bleh(targetResult, partialResult * numbers(position), numbers, position + 1, useConcatenate)
        || (useConcatenate && bleh(targetResult, concatenate(partialResult, numbers(position)), numbers, position + 1, useConcatenate))

  private def concatenate(first: Long, second: Long): Long =
    val multiplier = if second < 10 then 10
      else if second < 100 then 100
      else 1000
    first * multiplier + second
