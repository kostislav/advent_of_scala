package cz.judas.jan.advent

import java.nio.file.{Files, Paths}
import scala.annotation.StaticAnnotation
import scala.quoted.{Expr, Quotes, Type}

class pattern(val shape: String) extends StaticAnnotation

class InputData private(content: String):
  def lines: Iterator[String] =
    content.linesIterator

  def whole: String =
    content

  inline def linesAs[T]: Iterator[T] =
    ${ linesAsImpl[T]('{ this }) }

def linesAsImpl[T](input: Expr[InputData])(using Type[T])(using q: Quotes): Expr[Iterator[T]] =
  import q.reflect.*

  val parser = Expr.summon[StreamParsing[T]] match
    case Some(instance) => instance
    case None =>
      //given StreamParsing[Command] with
      //  override def parseFrom(input: ParseStream): Command =
      //    val direction = input.parse[Direction]()
      //    input.expect(" ")
      //    val amount = input.parse[Int]()
      //    Command(direction, amount)
      val t = q.reflect.TypeRepr.of[T]
      val classSymbol = t.classSymbol.get
      val patternType = TypeRepr.of[pattern]
      val patternAnnotation = classSymbol.annotations.filter { annotation => annotation.tpe =:= patternType }.headOption
      patternAnnotation match
        case Some(Apply(_, List(Literal(StringConstant(patternValue))))) =>
          val parts = splitAndKeepDelimiters(patternValue, "{}")
          val constructor = classSymbol.primaryConstructor

          val argumentParsers = constructor.paramSymss(0).map: nameSymbol =>
            t.memberType(nameSymbol).asType match
              case '[fieldType] => Expr.summon[StreamParsing[fieldType]].get

          '{
            new StreamParsing[T]:
              override def parseFrom(input: ParseStream): T = ${
                Apply(Select(New(TypeIdent(classSymbol)), constructor), argumentParsers.map(argParser => '{ ${argParser}.parseFrom(input) }.asTerm)).asExprOf[T]
              }
          }
        case _ => report.errorAndAbort(s"No @pattern annotation for type ${q.reflect.TypeRepr.of[T].typeSymbol}")

  '{ ParseStream(${input}.whole).parseLines(${parser}) }


object InputData:
  def real(year: Int, day: Int): InputData =
    val content = Files.readString(Paths.get(f"../advent_of_rust/input/year$year/day$day%02d"))
    InputData(content)

  def fromString(input: String): InputData =
    val lines = input.linesIterator.toSeq
    val trimmedLines = lines.slice(
      if (lines.head.isEmpty) 1 else 0,
      if (lines.last.chars().allMatch(_ == ' ')) lines.size - 1 else lines.size,
    )
    val numSpaces = trimmedLines.head.chars().takeWhile(_ == ' ').count().toInt
    InputData(trimmedLines.map(line => line.substring(numSpaces) + "\n").mkString)


class ParseStream(input: String):
  private var position = 0

  def parse[T]()(using streamParsing: StreamParsing[T]): T =
    streamParsing.parseFrom(this)

  def parseLines[T](streamParsing: StreamParsing[T]): Iterator[T] =
    LineIterator[T](this, streamParsing)

  def expect(value: String): Unit =
    if !tryConsume(value) then
      throw RuntimeException(s"Unexpected input at position ${position}")

  def hasNext: Boolean =
    position < input.length

  def peek: Char =
    input(position)

  def next(): Char =
    position += 1
    input(position - 1)

  def tryConsume(value: String): Boolean =
    if input.substring(position, position + value.length) != value then
      false
    else
      position += value.length
      true

  private class LineIterator[T](
    stream: ParseStream,
    parser: StreamParsing[T],
  ) extends Iterator[T]:
    override def hasNext: Boolean =
      stream.hasNext

    override def next(): T =
      val next = parser.parseFrom(stream)
      if hasNext then
        stream.expect("\n")
      next


trait StreamParsing[T]:
  def parseFrom(input: ParseStream): T


given StreamParsing[Int] with
  override def parseFrom(input: ParseStream): Int =
    val negative = if (input.peek == '-')
      input.next()
      true
    else
      false

    var result = 0
    while input.hasNext && ('0' to '9').contains(input.peek) do
      result = result * 10 + input.next() - '0'

    if negative then
      -result
    else
      result


private def splitAndKeepDelimiters(input: String, delimiter: String): Seq[String] =
  val parts = Seq.newBuilder[String]
  var position = 0
  while position < input.length do
    val next = input.indexOf(delimiter, position)
    if next != -1 then
      if next > position then
        parts += input.substring(position, next)
      parts += delimiter
      position += delimiter.length
    else
      if position < input.length - 1 then
        parts += input.substring(position)
      position = input.length

  parts.result()
