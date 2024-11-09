package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.{InputData, ParseStream, StreamParsing, given}

object Day02:
  def part1(input: InputData): Int =
    var horizontalPosition = 0
    var depth = 0

    for Command(direction, amount) <- input.linesAs[Command]() do
      direction match
        case Direction.Forward => horizontalPosition += amount
        case Direction.Down => depth += amount
        case Direction.Up => depth -= amount

    horizontalPosition * depth

  def part2(input: InputData): Int =
    var horizontalPosition = 0
    var depth = 0
    var aim = 0

    for Command(direction, amount) <- input.linesAs[Command]() do
      direction match
        case Direction.Down => aim += amount
        case Direction.Up => aim -= amount
        case Direction.Forward =>
          horizontalPosition += amount
          depth += aim * amount

    horizontalPosition * depth

enum Direction:
  case Up, Down, Forward

given StreamParsing[Direction] with
  override def parseFrom(input: ParseStream): Direction =
    if input.tryConsume("up") then
      Direction.Up
    else if input.tryConsume("down") then
      Direction.Down
    else if input.tryConsume("forward") then
      Direction.Forward
    else
      throw RuntimeException("Unexpected input")


case class Command(direction: Direction, amount: Int)

given StreamParsing[Command] with
  override def parseFrom(input: ParseStream): Command =
    val direction = input.parse[Direction]()
    input.expect(" ")
    val amount = input.parse[Int]()
    Command(direction, amount)
