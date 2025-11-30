package cz.judas.jan.advent.year2016

import com.google.common.hash.Hashing
import cz.judas.jan.advent.InputData

import java.nio.charset.StandardCharsets

object Day05:
  private val hexNumbers = "0123456789abcdef"

  def part1(input: InputData): String = {
    val doorId = input.whole.trim
    Iterator.unfold(0)(current => Some((current + 1, current + 1)))
      .flatMap: i =>
        val hashStart = Hashing.md5().hashString(s"${doorId}${i}", StandardCharsets.US_ASCII).asLong()
        if (hashStart & 0xF0FFFFL) == 0 then
          Some(hexNumbers(((hashStart >> 16) & 0xF).toInt))
        else
          None
      .take(8)
      .mkString("")
  }

  def part2(input: InputData): Int =
    0

