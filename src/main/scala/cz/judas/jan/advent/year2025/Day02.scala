package cz.judas.jan.advent.year2025

import com.google.common.math.LongMath
import cz.judas.jan.advent.{InputData, pattern, separatedBy}

import java.math.RoundingMode

object Day02:
  def part1(input: InputData): Long =
    input.wholeAs[Seq[Range] @separatedBy(",")]
      .flatMap: range =>
        val numDigitsLow = numDigits(range.low)
        val numDigitsHigh = numDigits(range.high)
        (numDigitsLow to numDigitsHigh)
          .map: numDigits =>
            if numDigits % 2 == 0 then
              val base = LongMath.pow(10, numDigits / 2)
              val start = if numDigits == numDigitsLow then
                val firstHalf = range.low / base
                val secondHalf = range.low % base
                if secondHalf <= firstHalf then
                  firstHalf
                else
                  firstHalf + 1
              else
                base / 10
              val end = if numDigits == numDigitsHigh then
                val firstHalf = range.high / base
                val secondHalf = range.high % base
                if secondHalf >= firstHalf then
                  firstHalf
                else
                  firstHalf - 1
              else
                base - 1

              (start + end) * (end - start + 1) / 2 * (base + 1)
            else
              0
      .sum

  def part2(input: InputData): Long =
    input.wholeAs[Seq[Range] @separatedBy(",")]
      .flatMap: range =>
        (range.low to range.high)
          .filter: number =>
            val numberString = number.toString
            val length = numberString.length
            (1 to length / 2)
              .exists(n => length % n == 0 && numberString == numberString.substring(0, n).repeat(length / n))
      .sum

  private def numDigits(n: Long): Int =
    LongMath.log10(n, RoundingMode.CEILING)


@pattern("{}-{}")
case class Range(low: Long, high: Long)