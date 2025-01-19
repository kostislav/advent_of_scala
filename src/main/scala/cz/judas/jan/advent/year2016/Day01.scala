package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{DirectionalPosition, InputData, Position, RelativePosition, pattern, separatedBy}

import scala.collection.mutable

object Day01:
  private val start = DirectionalPosition(Position(0, 0), RelativePosition.UP)

  def part1(input: InputData): Int =
    val finalPosition = parse(input)
      .foldLeft(start):
        case (position, (turn, distance)) =>
          turn(position).walk(distance)

    (finalPosition.position - start.position).manhattanDistance

  def part2(input: InputData): Int =
    val visited = mutable.HashSet[Position]()
    parse(input)
      .scanLeft(Seq(start)):
        case (previousLine, (turn, distance)) =>
          val newPosition = turn(previousLine.last)
          (1 to distance).map(newPosition.walk)
      .iterator
      .flatten
      .map(_.position)
      .flatMap: position =>
        if visited.contains(position) then
          Some((position - start.position).manhattanDistance)
        else
          visited.add(position)
          None
      .next()

  private def parse(input: InputData): Iterator[(Turn, Int)] =
    input.wholeAs[Iterator[(Turn, Int)] @separatedBy(", ")]

enum Turn:
  @pattern("R") case Right
  @pattern("L") case Left

  def apply(direction: DirectionalPosition): DirectionalPosition =
    this match
      case Turn.Right => direction.turnRight
      case Turn.Left => direction.turnLeft
