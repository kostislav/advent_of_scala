package cz.judas.jan.advent.year2016

import com.google.common.hash.Hashing
import cz.judas.jan.advent.{InputData, wholeNumbers}

import java.nio.charset.StandardCharsets
import scala.collection.mutable

object Day05:
  private val hexNumbers = "0123456789abcdef"

  def part1(input: InputData): String =
    val doorId = input.whole.trim
    passwordHashes(doorId)
      .take(8)
      .map(hashStart => hexDigit((hashStart >> 16) & 0xF))
      .mkString("")

  def part2(input: InputData): String =
    val doorId = input.whole.trim
    val password = mutable.TreeMap[Long, Char]()
    val hashIterator = passwordHashes(doorId)
    while password.size < 8 do
      val hashStart = hashIterator.next()
      val position = (hashStart >> 16) & 0xF
      if position < 8 then
        if !password.contains(position) then
          password.put(position, hexDigit((hashStart >> 28) & 0xF))

    password.values.mkString("")

  private def passwordHashes(doorId: String): Iterator[Long] =
    wholeNumbers(startingAt = 0)
      .flatMap: i =>
        val hashStart = Hashing.md5().hashString(s"${doorId}${i}", StandardCharsets.US_ASCII).asLong()
        if (hashStart & 0xF0FFFFL) == 0 then
          Some(hashStart)
        else
          None

  private def hexDigit(digit: Long): Char =
    hexNumbers(digit.toInt)

