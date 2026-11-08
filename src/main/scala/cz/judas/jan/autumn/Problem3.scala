package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData

import java.util.Base64
import scala.annotation.tailrec

object Problem3:
  def solve(input: InputData): Long =
    val file = Base64.getDecoder.decode(input.whole.trim).toList
    val (numEntries, remainingFile) = parseNumber(file.drop(4))
    processObjects(file, remainingFile, 0, numEntries)

  @tailrec
  private def parseNumber(data: List[Byte], acc: Long = 0): (Long, List[Byte]) =
    data match
      case n :: rest if n >= '0' && n <= '9' => parseNumber(rest, acc * 10 + (n - '0'))
      case rest => (acc, rest)

  @tailrec
  private def processObjects(wholeFile: List[Byte], file: List[Byte], index: Long, total: Long, acc: Long = 0): Long =
    index match
      case `total` => acc
      case _ =>
        val (offset, remainingFile) = parseNumber(file.tail)
        val (fp, _) = fingerprint(wholeFile.drop(offset.toInt))
        processObjects(wholeFile, remainingFile.drop(1), index + 1, total, acc + index * fp)

  private def fingerprint(file: List[Byte]): (Long, List[Byte]) =
    file match
      case 't' :: 'r' :: 'u' :: 'e' :: rest => (1, rest)
      case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest => (0, rest)
      case 'n' :: 'i' :: 'l' :: rest => (31, rest)
      case '"' :: rest => stringFingerprint(rest)
      case '[' :: rest => arrayFingerprint(rest)
      case '{' :: rest => mapFingerprint(rest)
      case rest => numberFingerprint(rest)

  @tailrec
  private def stringFingerprint(file: List[Byte], acc: Long = 0): (Long, List[Byte]) =
    file match
      case '\\' :: _ :: rest => stringFingerprint(rest, acc + 1)
      case '"' :: rest => (acc, rest)
      case _ :: rest => stringFingerprint(rest, acc + 1)

  @tailrec
  private def arrayFingerprint(file: List[Byte], acc: Long = 0): (Long, List[Byte]) =
    skipWhitespace(file) match
      case ';' :: rest => arrayFingerprint(rest, acc)
      case ']' :: rest => (1 + acc, rest)
      case rest =>
        val (itemFp, rest2) = fingerprint(rest)
        arrayFingerprint(rest2, acc + itemFp)

  @tailrec
  private def mapFingerprint(file: List[Byte], acc: Long = 0): (Long, List[Byte]) =
    skipWhitespace(file) match
      case '}' :: rest => (3 + acc, rest)
      case rest =>
        val (keyFp, afterKey) = fingerprint(skipWhitespace(file))
        val (valueFp, afterValue) = fingerprint(skipWhitespace(skipWhitespace(afterKey).drop(2)))
        val fp = keyFp * valueFp
        mapFingerprint(afterValue, acc + fp)

  private def numberFingerprint(file: List[Byte]): (Long, List[Byte]) =
    file match
      case '+' :: rest => parseNumber(rest)
      case '-' :: rest => parseNumber(rest)
      case rest => parseNumber(rest)

  @tailrec
  private def skipWhitespace(file: List[Byte]): List[Byte] =
    file match
      case ' ' :: rest => skipWhitespace(rest)
      case '\n' :: rest => skipWhitespace(rest)
      case rest => rest
