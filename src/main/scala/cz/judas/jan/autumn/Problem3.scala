package cz.judas.jan.autumn

import cz.judas.jan.advent.{InputData, generate}

import java.util.Base64

object Problem3:
  def solve(input: InputData): Long =
    val file = Base64.getDecoder.decode(input.whole.trim)
    val headerStream = ParseStream(file, 4)
    val numEntries = parseNumber(headerStream)
    generate(numEntries)(parseNumber(headerStream))
      .zipWithIndex
      .map: (startingOffset, i) =>
        i * fingerprint(ParseStream(file, startingOffset))
      .sum

  private def parseNumber(stream: ParseStream): Int =
    String(stream.takeWhile(_ != '\n').toArray).toInt

  private def fingerprint(stream: ParseStream): Long =
    stream.next() match
      case '{' =>
        skipWhitespace(stream)
        var result = 3L
        while stream.peek != '}' do
          val keyFingerprint = fingerprint(stream)
          skipWhitespace(stream)
          stream.skip(2)
          skipWhitespace(stream)
          val valueFingerprint = fingerprint(stream)
          skipWhitespace(stream)
          result += keyFingerprint * valueFingerprint
        stream.next()
        result
      case '[' =>
        skipWhitespace(stream)
        var result = 1L
        while stream.peek != ']' do
          result += fingerprint(stream)
          skipWhitespace(stream)
          if stream.peek == ';' then
            stream.next()
            skipWhitespace(stream)
        stream.next()
        result
      case '"' =>
        var result = 0L
        while stream.next() != '"' do
          result += 1
          if stream.current == '\\' then
            stream.next()
        result
      case 't' =>
        stream.skip(3)
        1
      case 'f' =>
        stream.skip(4)
        0
      case 'n' =>
        stream.skip(2)
        31
      case _ =>
        if stream.current == '+' || stream.current == '-' then
          stream.next()
        var result = (stream.current - '0').toLong
        while ('0' to '9').contains(stream.peek.toChar) do
          stream.next()
          result = result * 10 + stream.current - '0'
        result

  private def skipWhitespace(stream: ParseStream): Unit =
    while stream.peek == ' ' || stream.peek == '\n' do
      stream.next()


class ParseStream(
  data: Array[Byte],
  startingPos: Int,
) extends Iterator[Byte]:
  private var pos = startingPos

  def hasNext: Boolean =
    pos < data.length - 1

  def next(): Byte =
    pos += 1
    current

  def current: Byte =
    data(pos - 1)

  def peek: Byte =
    data(pos)

  def skip(n: Int): Unit =
    pos += n
