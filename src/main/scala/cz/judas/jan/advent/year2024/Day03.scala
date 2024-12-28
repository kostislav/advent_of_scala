package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, ParseStream, createParser, pattern}

object Day03:
  def part1(input: InputData): Int =
    instructionsFrom(input)
      .map:
        case Instruction.Mul(x, y) => x * y
        case _ => 0
      .sum

  def part2(input: InputData): Int =
    var result = 0
    var enabled = true

    instructionsFrom(input).foreach:
      case Instruction.Mul(x, y) =>
        if enabled then
          result += x * y
      case Instruction.Do => enabled = true
      case Instruction.Dont => enabled = false

    result

  private inline def instructionsFrom(input: InputData): Seq[Instruction] =
    val parser = createParser[Instruction]
    val stream = input.stream

    val result = Seq.newBuilder[Instruction]
    while stream.hasNext do
      parser.parseFrom(stream) match
        case Some(next) => result += next
        case None => stream.next()

    result.result()


enum Instruction:
  @pattern("mul({},{})") case Mul(x: Int, y: Int)
  @pattern("do()") case Do
  @pattern("don't()") case Dont
