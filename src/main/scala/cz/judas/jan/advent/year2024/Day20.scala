package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, selfProduct}

import scala.collection.mutable

object Day20:
  def part1(input: InputData): Int =
    solve(input, 2, 100)

  def part2(input: InputData): Int =
    solve(input, 20, 100)

  def solve(input: InputData, maximumCheatLength: Int, minimumSaved: Int): Int =
    val racetrack = input.asArray2d
    val start = racetrack.positionOfOnly('S')
    val end = racetrack.positionOfOnly('E')

    val distanceToEnd = distancesFrom(racetrack, end)
    val distanceFromStart = distancesFrom(racetrack, start)

    val regularPathLength = distanceToEnd(start)

    val allowedCheats = (-maximumCheatLength to maximumCheatLength).selfProduct(onlyDifferent = false)
      .map((rows, columns) => RelativePosition(rows, columns))
      .filter(cheat => cheat.manhattanDistance <= maximumCheatLength)

    distanceFromStart
      .map: (node, distance) =>
        allowedCheats.count: direction =>
          distanceToEnd.get(node + direction).exists(_ + distance + direction.manhattanDistance <= regularPathLength - minimumSaved)
      .sum

  private def distancesFrom(racetrack: Array2d, start: Position): Map[Position, Int] =
    val distanceFromStart = mutable.HashMap((start, 0))
    val toVisit = mutable.Queue((start, 0))
    while toVisit.nonEmpty do
      val (current, distance) = toVisit.dequeue()
      RelativePosition.horizontalDirections.foreach: direction =>
        val next = current + direction
        if racetrack(next) != '#' && !distanceFromStart.contains(next) then
          distanceFromStart.put(next, distance + 1)
          toVisit.enqueue((next, distance + 1))
    distanceFromStart.toMap
