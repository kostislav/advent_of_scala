package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, slidingTuples}

object Day06:
  def part1(input: InputData): Long =
    val lines = input.lines.toList
    val operationString = lines.last
    val numNumberLines = lines.length - 1
    val columnStarts = operationString.indices.filter(i => operationString(i) != ' ') ++ Seq(lines.map(_.length).max)
    columnStarts.slidingTuples
      .map: (columnStart, columnEnd) =>
        val numbers = lines.take(numNumberLines)
          .map(line => line.substring(columnStart, math.min(columnEnd, line.length)).trim.toLong)
        val operation = operationString(columnStart)
        if operation == '+' then
          numbers.sum
        else
          numbers.product
      .sum

  def part2(input: InputData): Long =
    val lines = input.lines.toList
    val operationString = lines.last
    val numNumberLines = lines.length - 1
    val columnStarts = operationString.indices.filter(i => operationString(i) != ' ') ++ Seq(lines.map(_.length).max)
    columnStarts.slidingTuples
      .map: (columnStart, columnEnd) =>
        val numberSquare = lines.take(numNumberLines)
          .map(line => line.substring(columnStart, math.min(columnEnd, line.length)) + " ".repeat(math.max(0, columnEnd - line.length)))
        val numbers = numberSquare.transpose.filterNot(_.forall(_ == ' ')).map(_.mkString("").trim.toLong)
        val operation = operationString(columnStart)
        if operation == '+' then
          numbers.sum
        else
          numbers.product
      .sum


