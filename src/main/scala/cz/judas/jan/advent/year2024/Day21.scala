package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, applyNTimes, cartesianProduct, transformValues}

object Day21:
  def part1(input: InputData): Long =
    solve(input, 2)

  def part2(input: InputData): Long =
    solve(input, 25)

  private def solve(input: InputData, numIntermediateRobots: Int): Long =
    val numericKeypad = Array2d.fromRows("789", "456", "123", ".0A").ignoring('.')
    val directionalKeypad = Array2d.fromRows(".^A", "<v>").ignoring('.')

    val directionalPaths = shortestPaths(directionalKeypad)
    val numericPaths = shortestPaths(numericKeypad)

    val finalCosts = applyNTimes(
      numIntermediateRobots,
      directionalPaths.keys.map(_ -> 1L).toMap
    ): costs =>
        directionalPaths.transformValues(applyRobot(_, directionalPaths, costs))

    input.lines
      .map: line =>
        val numPushes = applyToEachMove(line, ends => applyRobot(numericPaths(ends), directionalPaths, finalCosts))
        numPushes * line.replace("A", "").toInt
      .sum

  private def applyRobot(options: Seq[String], paths: Map[String, Seq[String]], costs: Map[String, Long]): Long =
    options
      .map(applyToEachMove(_, costs))
      .min

  private def applyToEachMove(moves: String, f: String => Long): Long =
    ("A" + moves)
      .sliding(2)
      .map(f)
      .sum

  private def shortestPaths(keypad: Array2d): Map[String, Seq[String]] =
    keypad.indices
      .toSeq
      .cartesianProduct(onlyDifferent = false)
      .map((start, end) => s"${keypad(start)}${keypad(end)}" -> possibleMoves(start, end, keypad))
      .toMap

  private def possibleMoves(start: Position, end: Position, keypad: Array2d): Seq[String] =
    if keypad.get(start).isEmpty then
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
