package cz.judas.jan.advent

import java.nio.file.{Files, Paths}
import scala.annotation.StaticAnnotation
import scala.collection.mutable
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
  LinesAsImpl().createTopLevelParser[T](input)

class LinesAsImpl(using q: Quotes):

  import q.reflect.*

  private val parseExprs = mutable.HashMap[Type[?], Term]()
  private val parseMethods = mutable.ListBuffer[DefDef]()

  def createTopLevelParser[T](input: Expr[InputData])(using Type[T]): Expr[Iterator[T]] =
    val parser = getOrCreateParser[T]().etaExpand(Symbol.spliceOwner).asExprOf[ParseStream => Option[T]]

    Block(
      parseMethods.toList,
      '{ ParseStream(${ input }.whole).parseLines(${ parser }) }.asTerm
    ).asExprOf[Iterator[T]]

  private def getOrCreateParser[T](using t: Type[T])(): Term =
    if !parseExprs.contains(t) then
      Expr.summon[StreamParsing[T]] match
        case Some(instance) => parseExprs.put(t, Select.unique(instance.asTerm, "parseFrom"))
        case None =>
          val typeRepr = TypeRepr.of[T]
          if typeRepr =:= TypeRepr.of[Int] then
            parseExprs.put(t, Select.unique('{ intParser }.asTerm, "parseFrom"))
          else
            val typeSymbol = typeRepr.typeSymbol

            val methodSymbol = Symbol.newMethod(
              Symbol.spliceOwner,
              s"f${parseMethods.size}",
              MethodType(List("input"))(
                _ => List(TypeRepr.of[ParseStream]),
                _ => TypeRepr.of[Option[T]]
              )
            )

            parseExprs.put(t, Ref(methodSymbol))

            parseMethods += DefDef(
              methodSymbol, {
                case List(List(input)) =>
                  val inputExpr = input.asExprOf[ParseStream]
                  val body =
                    val children = typeSymbol.children
                    if children.isEmpty then
                      patternAnnotation(typeSymbol) match
                        case Some(pattern) =>
                          caseClassParserBody(splitAndKeepDelimiters(pattern, "{}"), inputExpr, methodSymbol)
                        case None => report.errorAndAbort(s"No @pattern annotation for type ${typeSymbol}")
                    else
                      enumClassParserBody(inputExpr, methodSymbol)

                  Some(body)
                case _ => throw RuntimeException("WTF")
              }
            )

    parseExprs(t)

  private def caseClassParserBody[T](patternParts: Seq[String], input: Expr[ParseStream], enclosingMethod: Symbol)(using Type[T]): Term =
    val t = TypeRepr.of[T]
    val classSymbol = t.classSymbol.get
    val constructor = classSymbol.primaryConstructor

    caseClassParserBodyInternal[T](
      t,
      constructor.paramSymss(0).iterator,
      patternParts.iterator,
      List.empty,
      Select(New(TypeIdent(classSymbol)), constructor),
      input.asTerm,
      enclosingMethod,
    )

  private def caseClassParserBodyInternal[T](t: TypeRepr, fieldIterator: Iterator[Symbol], patternPartIterator: Iterator[String], variables: List[Symbol], constructor: Term, input: Term, enclosingMethod: Symbol)(using Type[T]): Term =
    if patternPartIterator.hasNext then
      val part = patternPartIterator.next()
      if part == "{}" then
        val fieldName = fieldIterator.next()
        val fieldType = t.memberType(fieldName)
        fieldType.asType match
          case '[fieldT] =>
            val variable = Symbol.newVal(enclosingMethod, s"v${variables.size}", TypeRepr.of[Option[fieldT]], Flags.EmptyFlags, Symbol.noSymbol)
            Block(
              List(
                ValDef(variable, Some(Apply(getOrCreateParser[fieldT](), List(input)).changeOwner(variable)))
              ),
              If(
                Select.unique(Ref(variable), "isDefined"),
                caseClassParserBodyInternal[T](t, fieldIterator, patternPartIterator, variable :: variables, constructor, input, enclosingMethod),
                '{ None }.asTerm,
              )
            )
      else
        If(
          Apply(Select.unique(input, "tryConsume"), List(Literal(StringConstant(part)))),
          caseClassParserBodyInternal[T](t, fieldIterator, patternPartIterator, variables, constructor, input, enclosingMethod),
          '{ None }.asTerm,
        )
    else
      some(Apply(constructor, variables.reverse.map(variable => Select.unique(Ref(variable), "get"))).asExprOf[T])

  private def enumClassParserBody[T](input: Expr[ParseStream], enclosingMethod: Symbol)(using Type[T]): Term =
    ifChain(
      TypeRepr.of[T].typeSymbol.children.map: child =>
        Apply(Select.unique(input.asTerm, "tryConsume"), List(Literal(StringConstant(child.name.toLowerCase)))) -> some(Ref(child).asExprOf[T]),
      '{ None }.asTerm
    )

  private def ifChain(branches: Seq[(Term, Term)], elseBranch: Term): Term =
    branches
      .reverse
      .foldLeft(elseBranch):
        case (rest, (condition, body)) => If(condition, body, rest)

  private def some[T](using Type[T])(value: Expr[T]): Term =
    '{ Some(${ value }) }.asTerm

  private def patternAnnotation(typeSymbol: q.reflect.Symbol): Option[String] =
    val patternType = TypeRepr.of[pattern]
    val patternAnnotation = typeSymbol.annotations.filter { annotation => annotation.tpe =:= patternType }.headOption
    patternAnnotation match
      case Some(Apply(_, List(Literal(StringConstant(patternValue))))) => Some(patternValue)
      case _ => None


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

  def parseLines[T](itemParser: ParseStream => Option[T]): Iterator[T] =
    LineIterator[T](this, itemParser)

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
    parser: ParseStream => Option[T],
  ) extends Iterator[T]:
    override def hasNext: Boolean =
      stream.hasNext

    override def next(): T =
      val next = parser(stream)
      if hasNext then
        stream.expect("\n")
      next.get


trait StreamParsing[T]:
  def parseFrom(input: ParseStream): Option[T]


given intParser: StreamParsing[Int] with
  private val digits = '0' to '9'

  override def parseFrom(input: ParseStream): Option[Int] =
    if input.peek == '-' then
      input.next()
      parsePositive(input).map(value => -value)
    else
      parsePositive(input)

  private def parsePositive(input: ParseStream): Option[Int] =
    var result = 0
    var foundDigit = false
    while input.hasNext && digits.contains(input.peek) do
      result = result * 10 + input.next() - '0'
      foundDigit = true

    if foundDigit then Some(result) else None


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
