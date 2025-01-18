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
    val path = parse(input)
      .scanLeft(Seq(start)):
        case (previousLine, (turn, distance)) =>
          val newPosition = turn(previousLine.last)
          (1 to distance).map(newPosition.walk)
      .iterator
      .flatten
    val visited = mutable.HashSet[Position]()
    var result: Option[Int] = None
    while path.hasNext && result.isEmpty do
      val next = path.next().position
      if visited.contains(next) then
        result = Some((next - start.position).manhattanDistance)
      else
        visited.add(next)
    result.get

  private def parse(input: InputData): Iterator[(Turn, Int)] =
    input.wholeAs[Iterator[(Turn, Int)] @separatedBy(", ")]

enum Turn:
  @pattern("R") case Right
  @pattern("L") case Left

  def apply(direction: DirectionalPosition): DirectionalPosition =
    this match
      case Turn.Right => direction.turnRight
      case Turn.Left => direction.turnLeft
