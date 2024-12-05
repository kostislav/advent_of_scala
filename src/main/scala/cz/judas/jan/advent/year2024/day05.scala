package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{AutoMap, InputData, pattern}

import scala.collection.mutable

object Day05:
  def part1(input: InputData): Int =
    val Array(orderingRulesString, updateString) = input.whole.split("\n\n")
    val orderingRulesBuilder = AutoMap[Int, mutable.HashSet[Int]](mutable.HashSet[Int]())
    InputData.fromString(orderingRulesString).linesAs[OrderingRule]
      .foreach(rule => orderingRulesBuilder.getOrCreate(rule.second).addOne(rule.first))
    val orderingRules = mutable.HashMap.newBuilder[Int, mutable.HashSet[Int]].addAll(orderingRulesBuilder.toMap).result()

    updateString.linesIterator
      .flatMap: line =>
        val numbers = line.split(",").map(_.toInt).toSeq
        val numbersSet = numbers.toSet
        val filteredRules = orderingRules.flatMap: (key, values) =>
          if numbersSet.contains(key) then
            Some(key -> values.intersect(numbersSet))
          else
            None
        val sorted = mutable.ArrayBuffer[Int]()
        while filteredRules.nonEmpty do
          val next = filteredRules.filter((key, values) => values.isEmpty)
          if next.size != 1 then
            throw RuntimeException("Boom")
          val nextNumber = next.iterator.next()._1
          sorted.addOne(nextNumber)
          filteredRules.remove(nextNumber)
          filteredRules.values.foreach(values => values.remove(nextNumber))

        if sorted == numbers then
          Some(sorted(sorted.size / 2))
        else
          None
      .sum

  def part2(input: InputData): Int =
    val Array(orderingRulesString, updateString) = input.whole.split("\n\n")
    val orderingRulesBuilder = AutoMap[Int, mutable.HashSet[Int]](mutable.HashSet[Int]())
    InputData.fromString(orderingRulesString).linesAs[OrderingRule]
      .foreach(rule => orderingRulesBuilder.getOrCreate(rule.second).addOne(rule.first))
    val orderingRules = mutable.HashMap.newBuilder[Int, mutable.HashSet[Int]].addAll(orderingRulesBuilder.toMap).result()

    updateString.linesIterator
      .flatMap: line =>
        val numbers = line.split(",").map(_.toInt).toSeq
        val numbersSet = numbers.toSet
        val filteredRules = orderingRules.flatMap: (key, values) =>
          if numbersSet.contains(key) then
            Some(key -> values.intersect(numbersSet))
          else
            None
        val sorted = mutable.ArrayBuffer[Int]()
        while filteredRules.nonEmpty do
          val next = filteredRules.filter((key, values) => values.isEmpty)
          if next.size != 1 then
            throw RuntimeException("Boom")
          val nextNumber = next.iterator.next()._1
          sorted.addOne(nextNumber)
          filteredRules.remove(nextNumber)
          filteredRules.values.foreach(values => values.remove(nextNumber))

        if sorted != numbers then
          Some(sorted(sorted.size / 2))
        else
          None
      .sum

@pattern("{}|{}")
case class OrderingRule(first: Int, second: Int)
