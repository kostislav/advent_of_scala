package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, regexMatches}

import java.util.regex.Pattern

object Day03:
  def part1(input: InputData): Int =
    regexMatches(Pattern.compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"), input.whole)
      .map(matched => matched.group(1).toInt * matched.group(2).toInt)
      .sum

  def part2(input: InputData): Int =
    var result = 0
    var enabled = true
    regexMatches(Pattern.compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)"), input.whole)
      .foreach: matched =>
        val instruction = matched.group(0)
        if instruction.startsWith("don") then
          enabled = false
        else if instruction.startsWith("do") then
          enabled = true
        else
          if enabled then
            result += matched.group(1).toInt * matched.group(2).toInt
    result
