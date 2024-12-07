package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

object Day07:
  def part1(input: InputData): Long =
    input.lines
      .flatMap: line =>
        val Array(testValueString, numberString) = line.split(": ")
        val testValue = testValueString.toLong
        val numbers = numberString.split(" ").map(_.toLong)
        if bleh(testValue, 0, numbers, 0) then
          Some(testValue)
        else
          None
      .sum

  def part2(input: InputData): Long =
    input.lines
      .flatMap: line =>
        val Array(testValueString, numberString) = line.split(": ")
        val testValue = testValueString.toLong
        val numbers = numberString.split(" ").map(_.toLong)
        if bleh2(testValue, 0, numbers, 0) then
          Some(testValue)
        else
          None
      .sum

  private def bleh(targetResult: Long, partialResult: Long, numbers: IndexedSeq[Long], position: Int): Boolean =
    if position == numbers.size then
      partialResult == targetResult
    else
      bleh(targetResult, partialResult + numbers(position), numbers, position + 1) || bleh(targetResult, partialResult * numbers(position), numbers, position + 1)

  private def bleh2(targetResult: Long, partialResult: Long, numbers: IndexedSeq[Long], position: Int): Boolean =
    if position == numbers.size then
      partialResult == targetResult
    else
      bleh2(targetResult, partialResult + numbers(position), numbers, position + 1)
        || bleh2(targetResult, partialResult * numbers(position), numbers, position + 1)
        || bleh2(targetResult, s"${partialResult}${numbers(position)}".toLong, numbers, position + 1)

