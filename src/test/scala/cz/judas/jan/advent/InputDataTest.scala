package cz.judas.jan.advent

import cz.judas.jan.advent.ParameterlessEnum.Down
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert


class InputDataTest:
  @Test
  def parsesInt(): Unit =
    val inputData = inputDataFromLines("12", "13")

    val parsedLines = inputData.linesAs[Int].toList

    assert(parsedLines == List(12, 13))

  @Test
  def parsesCaseClass(): Unit =
    @pattern("{} {}")
    case class Parsed(a: Int, b: Int)

    val inputData = inputDataFromLines("1 2", "10 20")

    val parsedLines = inputData.linesAs[Parsed].toList

    assert(parsedLines == List(Parsed(1, 2), Parsed(10, 20)))

  @Test
  def parsesParameterlessEnum(): Unit =
    val inputData = inputDataFromLines("up", "down")

    val parsedLines = inputData.linesAs[ParameterlessEnum].toList

    assert(parsedLines == List(ParameterlessEnum.Up, ParameterlessEnum.Down))

  @Test
  def parsesEnumWithSingleParameterVariants(): Unit =
    val inputData = inputDataFromLines("123", "down")

    val parsedLines = inputData.linesAs[EnumWithSingleParameterVariants].toList

    assert(parsedLines == List(EnumWithSingleParameterVariants.One(123), EnumWithSingleParameterVariants.Two(Down)))

  private def inputDataFromLines(lines: String*) =
    InputData.fromString(lines.mkString("\n"))


// cannot be inside parsesParameterlessEnum because https://github.com/scala/scala3/issues/20349
enum ParameterlessEnum:
  case Up, Down


enum EnumWithSingleParameterVariants:
  case One(value: Int)
  case Two(value: ParameterlessEnum)
