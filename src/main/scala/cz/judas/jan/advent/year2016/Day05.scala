package cz.judas.jan.advent.year2016

import com.google.common.hash.Hashing
import cz.judas.jan.advent.InputData

import java.nio.charset.StandardCharsets

object Day05:
  def part1(input: InputData): String = {
    val doorId = input.whole.trim
    Iterator.unfold(0)(current => Some((current + 1, current + 1)))
      .flatMap: i =>
        val hash = Hashing.md5().hashString(s"${doorId}${i}", StandardCharsets.UTF_8).toString
        if hash.startsWith("00000") then
          Some(hash(5))
        else
          None
      .take(8)
      .mkString("")
  }

  def part2(input: InputData): Int =
    0

