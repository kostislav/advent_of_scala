package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{AutoMap, InputData}

import scala.collection.mutable

object Day22:
  def part1(input: InputData): Long =
    input.linesAs[Long]
      .map: initial =>
        (0 until 2000).foldLeft(initial)((current, _) => generateNext(current))
      .sum

  def part2(input: InputData): Long =
    val pricesByDiffs = AutoMap[Seq[Int], Int](0)
    input.linesAs[Long]
      .foreach: initial =>
        val monkeyPricesByDiffs = mutable.HashMap[Seq[Int], Int]()
        var prev = initial
        var secret = generateNext(initial)
        val diff = mutable.Queue[Int]((secret % 10 - prev % 10).toInt)
        (1 until 2000).foreach: _ =>
          prev = secret
          secret = generateNext(secret)
          if diff.size == 4 then
            diff.dequeue()
          diff.enqueue((secret % 10 - prev % 10).toInt)
          if diff.size == 4 then
            val diffSeq = diff.toSeq
            if !monkeyPricesByDiffs.contains(diffSeq) then
              monkeyPricesByDiffs.put(diffSeq, (secret % 10).toInt)
        monkeyPricesByDiffs.foreach: (diff, price) =>
          pricesByDiffs.put(diff, pricesByDiffs.getOrCreate(diff) + price)

    pricesByDiffs.toMap.values.max

  private def generateNext(current: Long): Long =
    val temp1 = (current ^ (current << 6)) & 16777215
    val temp2 = (temp1 ^ (temp1 >> 5)) & 16777215
    (temp2 ^ (temp2 << 11)) & 16777215
