package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{AutoMap, InputData, pattern}

import scala.collection.mutable

object Day05:
  def part1(input: InputData): Int =
    val Array(orderingRulesString, updateString) = input.whole.split("\n\n")
    val orderingRulesBuilder = AutoMap[Int, mutable.Buffer[Int]](mutable.Buffer[Int]())
    InputData.fromString(orderingRulesString).linesAs[OrderingRule]
      .foreach(rule => orderingRulesBuilder.getOrCreate(rule.second).addOne(rule.first))
    val orderingRules = orderingRulesBuilder.toMap

    updateString.linesIterator
      .flatMap: line =>
        val numbers = line.split(",").map(_.toInt).toSeq
        val remaining = mutable.HashSet(numbers*)
        val valid = numbers.forall: number =>
          orderingRules.get(number) match
            case Some(rule) =>
              if orderingRules(number).exists(remaining.contains) then
                false
              else
                remaining.remove(number)
                true
            case None =>
              remaining.remove(number)
              true
        if valid then
          Some(numbers(numbers.size / 2))
        else
          None
      .sum

  def part2(input: InputData): Int =
    0

@pattern("{}|{}")
case class OrderingRule(first: Int, second: Int)
