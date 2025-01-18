package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{InputData, Position, RelativePosition, pattern, separatedBy}

import scala.collection.mutable

object Day01:
  private val start = Position(0, 0)

  def part1(input: InputData): Int =
    var currentPosition = start
    var currentDirection = RelativePosition.UP
    parse(input).foreach: (turn, distance) =>
      currentDirection = turn(currentDirection)
      currentPosition += currentDirection * distance

    (currentPosition - start).manhattanDistance

  def part2(input: InputData): Int =
    val visited = mutable.HashSet[Position]()
    var result: Option[Int] = None
    var currentPosition = start
    var currentDirection = RelativePosition.UP
    val path = parse(input)
    while result.isEmpty do
      val (turn, distance) = path.next()
      currentDirection = turn(currentDirection)
      (0 until distance).foreach: _ =>
        currentPosition += currentDirection
        if result.isEmpty then
          if visited.contains(currentPosition) then
            result = Some((currentPosition - start).manhattanDistance)
          else
            visited.add(currentPosition)
    result.get

  private def parse(input: InputData): Iterator[(Turn, Int)] =
    input.wholeAs[Iterator[(Turn, Int)] @separatedBy(", ")]

enum Turn:
  @pattern("R") case Right
  @pattern("L") case Left

  def apply(direction: RelativePosition): RelativePosition =
    this match
      case Turn.Right => direction.rotateRight
      case Turn.Left => direction.rotateLeft
