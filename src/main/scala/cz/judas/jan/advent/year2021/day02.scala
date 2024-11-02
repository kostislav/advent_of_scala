package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData

object Day02:
  def part1(input: InputData): Int =
    var horizontalPosition = 0
    var depth = 0

    for Command(direction, amount) <- input.lines.map(parseCommand) do
      direction match
        case Direction.Forward => horizontalPosition += amount
        case Direction.Down => depth += amount
        case Direction.Up => depth -= amount

    horizontalPosition * depth

  def part2(input: InputData): Int =
    var horizontalPosition = 0
    var depth = 0
    var aim = 0

    for Command(direction, amount) <- input.lines.map(parseCommand) do
      direction match
        case Direction.Down => aim += amount
        case Direction.Up => aim -= amount
        case Direction.Forward =>
          horizontalPosition += amount
          depth += aim * amount

    horizontalPosition * depth

def parseCommand(line: String): Command =
  val Array(direction, amount) = line.split(' ')
  Command(Direction.valueOf(direction.capitalize), amount.toInt)

enum Direction:
  case Up, Down, Forward

case class Command(direction: Direction, amount: Int)
