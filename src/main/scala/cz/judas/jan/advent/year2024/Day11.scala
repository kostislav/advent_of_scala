package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

import scala.collection.mutable

object Day11:
  def part1(input: InputData): Long =
    val memory = mutable.HashMap[(Long, Int), Long]()
    input.whole.split(' ').map(_.toLong)
      .map(stone => bleh(25, stone, memory))
      .sum

  def part2(input: InputData): Long =
    val memory = mutable.HashMap[(Long, Int), Long]()
    input.whole.split(' ').map(_.toLong)
      .map(stone => bleh(75, stone, memory))
      .sum

  private def bleh(remaining: Int, stone: Long, memory: mutable.HashMap[(Long, Int), Long]): Long =
    if remaining == 0 then
      1
    else
      memory.get((stone, remaining)) match
        case Some(number) => number
        case None =>
          if stone == 0 then
            bleh(remaining - 1, 1, memory)
          else
            val digits = stone.toString
            if digits.length % 2 == 0 then
              val half = digits.length / 2
              val number = bleh(remaining - 1, digits.substring(0, half).toLong, memory) + bleh(remaining - 1, digits.substring(half).toLong, memory)
              memory.put((stone, remaining), number)
              number
            else
              val number = bleh(remaining - 1, stone * 2024, memory)
              memory.put((stone, remaining), number)
              number

