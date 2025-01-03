package cz.judas.jan.advent

import cz.judas.jan.advent.EnumWithParameters.One
import org.junit.jupiter.api.function.Executable
import org.junit.jupiter.api.{Assertions, Test}
import org.scalatest.Assertions.assert


class InputDataTest:
  @Test
  def parsesInt(): Unit =
    val inputData = inputDataFromLines("12", "13")

    val parsedLines = inputData.linesAs[Int].toList

    assert(parsedLines == List(12, 13))

  @Test
  def parsesLong(): Unit =
    val inputData = inputDataFromLines("100000000000", "13")

    val parsedLines = inputData.linesAs[Long].toList

    assert(parsedLines == List(100000000000L, 13))

  @Test
  def intParsingFailsForTooBigValue(): Unit =
    val inputData = inputDataFromLines("100000000000")

    Assertions.assertThrows(classOf[RuntimeException], () => inputData.linesAs[Int].toList)

  @Test
  def parsesCaseClass(): Unit =
    @pattern("{} {}")
    case class Parsed(a: Int, b: Int)

    val inputData = inputDataFromLines("1 2", "10 20")

    val parsedLines = inputData.linesAs[Parsed].toList

    assert(parsedLines == List(Parsed(1, 2), Parsed(10, 20)))

  @Test
  def parsesEnum(): Unit =
    val inputData = inputDataFromLines("one", "1234", "complex 10 one")

    val parsedLines = inputData.linesAs[EnumWithParameters].toList

    assert(parsedLines == List(EnumWithParameters.One, EnumWithParameters.Two(1234), EnumWithParameters.Three(10, EnumWithParameters.One)))

  @Test
  def parsesParameterlessEnumWithCustomNames(): Unit =
    val inputData = inputDataFromLines("pup", "pown")

    val parsedLines = inputData.linesAs[CustomParameterlessEnum].toList

    assert(parsedLines == List(CustomParameterlessEnum.Up, CustomParameterlessEnum.Down))

  @Test
  def backtracksWhileParsingEnum(): Unit =
    val inputData = inputDataFromLines("3 blah", "5 bleh")

    val parsedLines = inputData.linesAs[EnumWithBacktracking].toList

    assert(parsedLines == List(EnumWithBacktracking.Two(3), EnumWithBacktracking.One(5)))

  private def inputDataFromLines(lines: String*) =
    InputData.fromString(lines.mkString("\n"))


class SplitAndKeepDelimitersTest:
  @Test
  def startingAndEndingWithDelimiter(): Unit =
    val parts = splitAndKeepDelimiters("{}, {} and {}", "{}")

    assert(parts == List("{}", ", ", "{}", " and ", "{}"))

  @Test
  def startingWithDelimiter(): Unit =
    val parts = splitAndKeepDelimiters("{} rocks", "{}")

    assert(parts == List("{}", " rocks"))

  @Test
  def endingWithDelimiter(): Unit =
    val parts = splitAndKeepDelimiters("looking for {}", "{}")

    assert(parts == List("looking for ", "{}"))

  @Test
  def endingWithSingleChar(): Unit =
    val parts = splitAndKeepDelimiters("looking for ({})", "{}")

    assert(parts == List("looking for (", "{}", ")"))


enum ParameterlessEnum:
  case Up, Down


enum CustomParameterlessEnum:
  @pattern("pup") case Up
  @pattern("pown") case Down


// cannot be inside parsesEnum because https://github.com/scala/scala3/issues/20349
enum EnumWithParameters:
  case One
  case Two(value: Int)
  @pattern("complex {} {}") case Three(value1: Int, value2: EnumWithParameters)


enum EnumWithBacktracking:
  @pattern("{} bleh") case One(value: Int)
  @pattern("{} blah") case Two(value: Int)
