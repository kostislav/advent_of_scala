package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, pattern, separatedBy}

import scala.collection.mutable

object Day17:
  def part1(input: InputData): String =
    val program = input.wholeAs[Program]
    val instructions = program.instructions
    var a = program.a
    var b = program.b
    var c = program.c
    var ip = 0
    val result = mutable.ArrayBuffer[Int]()

    while ip < instructions.size do
      val operandOpcode = instructions(ip + 1)
      instructions(ip) match
        case 0 => a = a >> comboOperand(operandOpcode, a, b, c)
        case 1 => b = b ^ operandOpcode
        case 2 => b = comboOperand(operandOpcode, a, b, c) & 7
        case 3 =>
          if a != 0 then
            ip = operandOpcode - 2
        case 4 => b = b ^ c
        case 5 => result += comboOperand(operandOpcode, a, b, c) & 7
        case 6 => b = a >> comboOperand(operandOpcode, a, b, c)
        case 7 => c = a >> comboOperand(operandOpcode, a, b, c)

      ip += 2

    result.mkString(",")

  def part2(input: InputData): Long =
    val program = input.wholeAs[Program]
    val instructions = program.instructions
    bleh(0, instructions.size - 1, instructions).get

  private def comboOperand(opcode: Int, a: Int, b: Int, c: Int): Int =
    opcode match
      case 4 => a
      case 5 => b
      case 6 => c
      case 7 => throw RuntimeException("Should not happen")
      case x => x

  private def bleh(a: Long, i: Int, program: IndexedSeq[Int]): Option[Long] =
    if i == -1 then
      Some(a)
    else
      (0 to 7)
        .flatMap: n =>
          val newA = (a << 3) + n
          if ((newA ^ (newA >> (n ^ 7))) & 7) == program(i) then
            bleh(newA, i - 1, program)
          else
            None
        .minOption

@pattern("Register A: {}\nRegister B: {}\nRegister C: {}\n\nProgram: {}")
case class Program(a: Int, b: Int, c: Int, instructions: IndexedSeq[Int] @separatedBy(","))
