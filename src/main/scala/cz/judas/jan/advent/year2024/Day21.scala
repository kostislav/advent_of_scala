package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, cartesianProduct}

object Day21:
  def part1(input: InputData): Long =
    solve(input, 2)

  def part2(input: InputData): Long =
    solve(input, 25)

  private def solve(input: InputData, numIntermediateRobots: Int): Long =
    val numericKeypad = Array2d.fromRows("789", "456", "123", ".0A")
    val directionalKeypad = Array2d.fromRows(".^A", "<v>")

    val transitions = Seq('<', '>', '^', 'v', 'A')
      .cartesianProduct(onlyDifferent = false)
      .map((start, end) => s"${start}${end}" -> possibleMoves(directionalKeypad.positionOfOnly(start), directionalKeypad.positionOfOnly(end), directionalKeypad))
      .toMap

    val finalCosts = (0 until numIntermediateRobots)
      .foldLeft(transitions.view.mapValues(_ => 1L).toMap): (costs, _) =>
        transitions
          .view.mapValues: options =>
            options
              .map: moves =>
                ("A" + moves).sliding(2).map(ends => costs(ends)).sum
              .min
          .toMap

    input.lines
      .map: line =>
        val numPushes = ("A" + line)
          .sliding(2)
          .map: ends =>
            possibleMoves(numericKeypad.positionOfOnly(ends(0)), numericKeypad.positionOfOnly(ends(1)), numericKeypad)
              .map: option =>
                ("A" + option).sliding(2).map(ends => finalCosts(ends)).sum
              .min
          .sum  // TODO dedup
        numPushes * line.replace("A", "").toInt
      .sum

  private def possibleMoves(start: Position, end: Position, keypad: Array2d): Seq[String] =
    if keypad(start) == '.' then
      Seq.empty
    else
      if start == end then
        Seq("A")
      else
        val result = Seq.newBuilder[String]
        if start.column < end.column then
          result ++= possibleMoves(start + RelativePosition.RIGHT, end, keypad).map(">" + _)
        if start.column > end.column then
          result ++= possibleMoves(start + RelativePosition.LEFT, end, keypad).map("<" + _)
        if start.row < end.row then
          result ++= possibleMoves(start + RelativePosition.DOWN, end, keypad).map("v" + _)
        if start.row > end.row then
          result ++= possibleMoves(start + RelativePosition.UP, end, keypad).map("^" + _)
        result.result()
