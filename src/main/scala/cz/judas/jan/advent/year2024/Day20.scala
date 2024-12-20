package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, cartesianProduct, getOnlyElement}

import scala.collection.mutable

object Day20:
  def part1(input: InputData): Int =
    part1(input, 100)

  def part1(input: InputData, minimumSaved: Int): Int =
    val racetrack = Array2d.fromInput(input)
    val start = racetrack.entries.filter(_._2 == 'S').map(_._1).getOnlyElement // TODO dedup
    val end = racetrack.entries.filter(_._2 == 'E').map(_._1).getOnlyElement // TODO dedup

    val distanceToEnd = distancesFrom(racetrack, end)
    val distanceFromStart = distancesFrom(racetrack, start)

    val regularPathLength = distanceToEnd(start)

    distanceFromStart
      .map: (node, distance) =>
        RelativePosition.horizontalDirections.count: direction =>
          distanceToEnd.get(node + direction * 2).exists(_ + distance + 2 <= regularPathLength - minimumSaved)
      .sum

  def part2(input: InputData): Int =
    part2(input, 100)

  def part2(input: InputData, minimumSaved: Int): Int =
    val racetrack = Array2d.fromInput(input)
    val start = racetrack.entries.filter(_._2 == 'S').map(_._1).getOnlyElement // TODO dedup
    val end = racetrack.entries.filter(_._2 == 'E').map(_._1).getOnlyElement // TODO dedup

    val distanceToEnd = distancesFrom(racetrack, end)
    val distanceFromStart = distancesFrom(racetrack, start)

    val regularPathLength = distanceToEnd(start)

    val allowedCheats = (-20 to 20).cartesianProduct(onlyDifferent = false)
      .map((rows, columns) => RelativePosition(rows, columns))
      .filter(cheat => cheat.manhattanDistance <= 20)

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
