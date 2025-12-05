package cz.judas.jan.advent.year2025

import com.google.common.math.LongMath
import cz.judas.jan.advent.{InclusiveRange, InputData, separatedBy}

import java.math.RoundingMode

object Day02:
  def part1(input: InputData): Long =
    input.wholeAs[Seq[InclusiveRange] @separatedBy(",")]
      .flatMap: range =>
        val numDigitsLow = numDigits(range.start)
        val numDigitsHigh = numDigits(range.end)
        (numDigitsLow to numDigitsHigh)
          .map: numDigits =>
            if numDigits % 2 == 0 then
              val base = LongMath.pow(10, numDigits / 2)

              val start = if numDigits == numDigitsLow then
                (range.start + base - 1 - range.start / base) / base
              else
                base / 10

              val end = if numDigits == numDigitsHigh then
                  (range.end - range.end / base) / base
              else
                base - 1

              (start + end) * (end - start + 1) / 2 * (base + 1)
            else
              0
      .sum

  def part2(input: InputData): Long =
    input.wholeAs[Seq[InclusiveRange] @separatedBy(",")]
      .flatMap: range =>
        (range.start to range.end)
          .filter: number =>
            val numberString = number.toString
            val length = numberString.length
            (1 to length / 2)
              .exists(n => length % n == 0 && numberString == numberString.substring(0, n).repeat(length / n))
      .sum

  private def numDigits(n: Long): Int =
    LongMath.log10(n, RoundingMode.CEILING)
