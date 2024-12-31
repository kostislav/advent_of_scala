package cz.judas.jan.advent

import java.nio.file.{Files, Path, Paths}
import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

class pattern(val shape: String) extends StaticAnnotation

class separatedBy(val separator: String) extends StaticAnnotation

class InputData(content: String):
  def lines: Iterator[String] =
    content.linesIterator

  def whole: String =
    content

  def stream: ParseStream =
    ParseStream(whole)

  def asArray2d: Array2d =
    Array2d.fromRows(lines.toIndexedSeq)

  inline def linesAs[T]: Iterator[T] =
    ChunkIterator.ofLines(stream, createParser[T])

  inline def wholeAs[T]: T =
    parseStructured(createParser[T])

  def parseStructured[T](parser: StreamParsing[T]): T =
    parser.parseFrom(stream).get

  def parseStructured[A, B](headerParser: StreamParsing[A], restParser: StreamParsing[B]): (A, B) =
    val stream = this.stream
    val header = headerParser.parseFrom(stream).get
    val rest = restParser.parseFrom(stream).get
    (header, rest)

  inline def parseStructuredInto[T <: Product](headerParser: StreamParsing[?], restParser: StreamParsing[?])(using mirror: Mirror.ProductOf[T]): T =
    val parsed = parseStructured(headerParser, restParser)
    mirror.fromProduct(parsed)


inline def headerOf[T]: StreamParsing[T] =
  headerWith(createParser[T])

inline def headerWith[T](parser: StreamParsing[T]): StreamParsing[T] =
  HeaderParser(parser)

inline def blocksOf[T]: StreamParsing[Iterator[T]] =
  BlockParser(createParser[T])

inline def linesOf[T]: StreamParsing[IndexedSeq[T]] =
  SeqParser(createParser[T], "\n")

inline def lazyLinesOf[T]: StreamParsing[Iterator[T]] =
  LazyLineParser(createParser[T])

def rawLines: StreamParsing[Iterator[String]] =
  RawLinesParser


inline def createParser[T]: StreamParsing[T] =
  ${ createParserImpl[T] }

def createParserImpl[T](using Type[T])(using q: Quotes): Expr[StreamParsing[T]] =
  ParsingMacros().createParserInstance[T]

class ParsingMacros(using q: Quotes):

  import q.reflect.*

  private val parseExprs = mutable.HashMap[TypeRef, Term]()
  private val parseMethods = mutable.ArrayBuffer[DefDef]()

  def createParserInstance[T](using Type[T]): Expr[StreamParsing[T]] =
    val parser = getOrCreateParser(TypeRef.from(TypeRepr.of[T]))

    Block(
      parseMethods.toList,
      '{ FunctionBasedStreamParsing(${ parser.etaExpand(Symbol.spliceOwner).asExprOf[ParseStream => Option[T]] }) }.asTerm
    ).asExprOf[StreamParsing[T]]

  private def getOrCreateParser(tpe: TypeRef): Term =
    if !parseExprs.contains(tpe) then
      if tpe == TypeRef.unannotated[Int] then
        parseExprs.put(tpe, Select.unique('{ intParser }.asTerm, "parseFrom"))
      else if tpe == TypeRef.unannotated[Long] then
        parseExprs.put(tpe, Select.unique('{ longParser }.asTerm, "parseFrom"))
      else if tpe == TypeRef.unannotated[String] then
        parseExprs.put(tpe, Select.unique('{ WordParser }.asTerm, "parseFrom"))
      else
        val methodSymbol = Symbol.newMethod(
          Symbol.spliceOwner,
          s"f${parseMethods.size}",
          MethodType(List("input"))(
            _ => List(TypeRepr.of[ParseStream]),
            _ => option(tpe.tpe)
          )
        )

        parseExprs.put(tpe, Ref(methodSymbol))

        parseMethods += DefDef(
          methodSymbol, {
            case List(List(input)) =>
              val inputTerm = input.asExprOf[ParseStream].asTerm
              val body =
                tpe.tpe match
                  case classType: ClassType =>
                    if SimpleType.of[IndexedSeq[Nothing]].isSubclassOf(tpe.tpe) then
                      val separatedBy = tpe.annotations.find(_.name == "separatedBy").get.parameters.head
                      val itemType = classType.typeArg(0)
                      val itemParser = getOrCreateParser(itemType)
                      val itemTypeRepr = itemType.tpe.asTypeRepr
                      Apply(
                        Select.unique(
                          instantiate(
                            TypeRepr.of[SeqParser].appliedTo(itemTypeRepr),
                            List(
                              instantiate(
                                TypeRepr.of[FunctionBasedStreamParsing].appliedTo(itemTypeRepr),
                                List(
                                  itemParser.etaExpand(methodSymbol),
                                )
                              ),
                              Literal(StringConstant(separatedBy)),
                            )
                          ),
                          "parseFrom"
                        ),
                        List(inputTerm)
                      )
                    else
                      val constructor = classType.constructor
                      val maybePattern = (tpe.annotations ++ classType.annotations).find(_.name == "pattern").map(_.parameters.head)
                      val pattern = maybePattern.map(patternString => splitAndKeepDelimiters(patternString, "{}")).getOrElse(Seq.fill(constructor.parameters.size)("{}"))
                      caseClassParserBody(
                        constructor.parameters.iterator,
                        pattern.iterator,
                        List.empty,
                        constructor,
                        inputTerm,
                        methodSymbol,
                      )
                  case unionType: UnionType =>
                    unionType.options
                      .reverse
                      .foldLeft('{ None }.asTerm):
                        case (rest, child) =>
                          val variable = Symbol.newVal(methodSymbol, "v", option(tpe.tpe), Flags.EmptyFlags, Symbol.noSymbol)
                          Block(
                            List(
                              ValDef(
                                variable,
                                Some(
                                  Apply(TypeApply(Select.unique(inputTerm, "tryParse"), List(Inferred(tpe.tpe.asTypeRepr))), List(getOrCreateParser(child).etaExpand(methodSymbol))).changeOwner(variable)
                                )
                              ),
                            ),
                            If(
                              Select.unique(Ref(variable), "isDefined"),
                              some(Select.unique(Ref(variable), "get"), tpe.tpe),
                              rest
                            )
                          )
                  case objectType: ObjectType =>
                    val name = (tpe.annotations ++ objectType.annotations).find(_.name == "pattern").map(_.parameters.head).getOrElse(objectType.instance.name.toLowerCase)
                    If(
                      Apply(Select.unique(inputTerm, "tryConsume"), List(Literal(StringConstant(name)))),
                      some(Ref(objectType.instance), tpe.tpe),
                      '{ None }.asTerm
                    )

              Some(body)
            case _ => throw RuntimeException("WTF")
          }
        )

    parseExprs(tpe)

  private def caseClassParserBody(parameterIterator: Iterator[Parameter], patternPartIterator: Iterator[String], variables: List[Symbol], constructor: Method, input: Term, enclosingMethod: Symbol): Term =
    if patternPartIterator.hasNext then
      val part = patternPartIterator.next()
      if part == "{}" then
        val parameter = parameterIterator.next()
        val variable = Symbol.newVal(enclosingMethod, s"v${variables.size}", option(parameter.tpe.tpe), Flags.EmptyFlags, Symbol.noSymbol)
        Block(
          List(
            ValDef(variable, Some(Apply(getOrCreateParser(parameter.tpe), List(input)).changeOwner(variable)))
          ),
          If(
            Select.unique(Ref(variable), "isDefined"),
            caseClassParserBody(parameterIterator, patternPartIterator, variable :: variables, constructor, input, enclosingMethod),
            '{ None }.asTerm,
          )
        )
      else
        If(
          Apply(Select.unique(input, "tryConsume"), List(Literal(StringConstant(part)))),
          caseClassParserBody(parameterIterator, patternPartIterator, variables, constructor, input, enclosingMethod),
          '{ None }.asTerm,
        )
    else
      some(constructor.call(variables.reverse.map(variable => Select.unique(Ref(variable), "get"))), constructor.returnType)

  private def option(itemType: SimpleType): TypeRepr =
    TypeRepr.of[Option].appliedTo(itemType.asTypeRepr)

  private def some(value: Term, tpe: SimpleType): Term =
    instantiate(TypeRepr.of[Some].appliedTo(tpe.asTypeRepr), List(value))

  private def instantiate(targetType: TypeRepr, args: List[Term]): Term =
    val typeSymbol = targetType.typeSymbol
    val constructor = Select(New(TypeIdent(typeSymbol)), typeSymbol.primaryConstructor)
    val constructorMethod = if targetType.typeArgs.nonEmpty then
      TypeApply(
        constructor,
        targetType.typeArgs.map(Inferred(_)),
      )
    else
      constructor
    Apply(constructorMethod, args)

  private sealed trait SimpleType:
    def asTypeRepr: TypeRepr

    def isSubclassOf(other: SimpleType): Boolean =
      asTypeRepr <:< other.asTypeRepr

  private object SimpleType:
    def of[T](using Type[T]): SimpleType = {
      val typeRepr = TypeRepr.of[T]
      //      if typeRepr.isInstanceOf[AnnotatedType] then
      //        report.errorAndAbort("Trying to create a simple type from an annotated type")
      of(typeRepr)
    }

    def of(typeRepr: TypeRepr): SimpleType =
      val children = typeRepr.typeSymbol.children
      if children.nonEmpty then
        val childTypes = children.map: child =>
          if child.isType then
            TypeRef.from(child.typeRef)
          else
            TypeRef(ObjectType(child), List.empty)
        UnionType(typeRepr, childTypes)
      else
        typeRepr match
          case OrType(left, right) =>
            UnionType(typeRepr, List(TypeRef.from(left), TypeRef.from(right)))
          case _ =>
            ClassType(typeRepr)

  private class ClassType(private val typeRepr: TypeRepr) extends SimpleType:
    private val typeSymbol = typeRepr.typeSymbol

    def asTypeRepr: TypeRepr =
      typeRepr

    def constructor: Method =
      val typeArgs = typeRepr.typeArgs
      val primaryConstructor = typeSymbol.primaryConstructor
      if typeRepr.isTupleN then
        val parameters = primaryConstructor.paramSymss(1).zip(typeArgs).map: (sym, typeArg) =>
          Parameter(sym.name, TypeRef.from(typeArg))
        Method(TypeApply(Select(New(TypeIdent(typeSymbol)), primaryConstructor), typeArgs.map(Inferred(_))), parameters, this)
      else
        val parameters = primaryConstructor.paramSymss(0).map: sym =>
          Parameter(sym.name, TypeRef.from(typeRepr.memberType(sym)))
        Method(Select(New(TypeIdent(typeSymbol)), primaryConstructor), parameters, this)

    def annotations: List[Annotation] =
      typeSymbol.annotations.flatMap(Annotation.from)

    def typeArg(index: Int): TypeRef =
      TypeRef.from(typeRepr.typeArgs(index))

    override def equals(obj: Any): Boolean =
      obj != null && obj.isInstanceOf[ClassType] && obj.asInstanceOf[ClassType].typeRepr =:= typeRepr

    override def hashCode(): Int =
      typeSymbol.hashCode

    override def toString: String =
      typeRepr.toString

  private class UnionType(private val original: TypeRepr, val options: List[TypeRef]) extends SimpleType:
    def asTypeRepr: TypeRepr =
      original

  private case class ObjectType(instance: Symbol) extends SimpleType:
    def asTypeRepr: TypeRepr =
      instance.typeRef

    def annotations: List[Annotation] =
      asTypeRepr.typeSymbol.annotations.flatMap(Annotation.from)

  private class Method(application: Term, val parameters: List[Parameter], val returnType: SimpleType):
    def call(args: List[Term]): Term =
      Apply(application, args)

  private case class Annotation(name: String, parameters: List[String])

  private case class TypeRef(tpe: SimpleType, annotations: List[Annotation])

  private object TypeRef:
    def from(typeRepr: TypeRepr): TypeRef =
      typeRepr match
        case AnnotatedType(underlying, annotation) =>
          TypeRef(SimpleType.of(underlying), List(Annotation.from(annotation).get))
        case unannotated =>
          this.unannotated(unannotated)

    def unannotated[T](using Type[T]): TypeRef =
      unannotated(TypeRepr.of[T])

    def unannotated(typeRepr: TypeRepr): TypeRef =
      TypeRef(SimpleType.of(typeRepr), List.empty)

  private object Annotation:
    def from(term: Term): Option[Annotation] =
      term match
        case Apply(Select(New(name), _), annotationParams) =>
          Some(
            Annotation(
              name.tpe.typeSymbol.name,
              annotationParams.map:
                case Literal(StringConstant(value)) => value
            )
          )
        case _ => None

  private case class Parameter(name: String, tpe: TypeRef)

object InputData:
  def real(year: Int, day: Int): InputData =
    val content = Files.readString(inputLocation(year, day))
    InputData(content)

  def inputLocation(year: Int, day: Int): Path =
    Paths.get(f"input/year$year/day${day}%02d")

  def fromString(input: String): InputData =
    val lines = input.linesIterator.toSeq
    if lines.size == 1 then
      InputData(input)
    else
      val trimmedLines = lines.slice(
        if (lines.head.isEmpty) 1 else 0,
        if (lines.last.chars().allMatch(_ == ' ')) lines.size - 1 else lines.size,
      )
      if trimmedLines.isEmpty then
        InputData("")
      else
        val numSpaces = trimmedLines.head.chars().takeWhile(_ == ' ').count().toInt
        InputData(trimmedLines.map(line => (if line.isEmpty then line else line.substring(numSpaces)) + "\n").mkString)


class ParseStream(input: String):
  private var position = 0

  def expect(value: String): Unit =
    if !tryConsume(value) then
      throw RuntimeException(s"Unexpected input at position ${position}")

  def hasNext: Boolean =
    hasNext(1)

  def hasNext(n: Int): Boolean =
    position + n <= input.length

  def peek: Char =
    input(position)

  def next(): Char =
    position += 1
    input(position - 1)

  def tryConsume(value: String): Boolean =
    if input.startsWith(value, position) then
      position += value.length
      true
    else
      false

  def tryParse[T](parser: ParseStream => Option[T]): Option[T] =
    val currentPosition = position
    val result = parser(this)
    if result.isEmpty then
      position = currentPosition
    result

  def consumeWhile(predicate: Char => Boolean): String =
    val originalPosition = position
    while hasNext && predicate(input(position)) do
      position += 1
    input.substring(originalPosition, position)


private class ChunkIterator[T](
  stream: ParseStream,
  parser: StreamParsing[T],
  delimiter: String,
) extends Iterator[T]:
  override def hasNext: Boolean =
    stream.hasNext(delimiter.length)

  override def next(): T =
    val next = parser.parseFrom(stream)
    if hasNext then
      stream.expect(delimiter)
    next.get

object ChunkIterator:
  def ofLines[T](stream: ParseStream, parser: StreamParsing[T]): ChunkIterator[T] =
    ChunkIterator(stream, parser, "\n")

  def ofBlocks[T](stream: ParseStream, parser: StreamParsing[T]): ChunkIterator[T] =
    ChunkIterator(stream, parser, "\n\n")


trait StreamParsing[T]:
  def parseFrom(input: ParseStream): Option[T]


class FunctionBasedStreamParsing[T](
  function: ParseStream => Option[T]
) extends StreamParsing[T]:
  override def parseFrom(input: ParseStream): Option[T] =
    function(input)


given longParser: StreamParsing[Long] with
  private val digits = '0' to '9'

  override def parseFrom(input: ParseStream): Option[Long] =
    if input.hasNext && input.peek == '-' then
      input.next()
      parsePositive(input).map(value => -value)
    else
      parsePositive(input)

  private def parsePositive(input: ParseStream): Option[Long] =
    var result = 0L
    var foundDigit = false
    while input.hasNext && digits.contains(input.peek) do
      result = result * 10 + input.next() - '0'
      foundDigit = true

    if foundDigit then Some(result) else None


given intParser: StreamParsing[Int] with
  override def parseFrom(input: ParseStream): Option[Int] =
    longParser.parseFrom(input)
      .map: value =>
        if value > Int.MaxValue then
          throw RuntimeException("Value too big")
        else
          value.toInt


def splitAndKeepDelimiters(input: String, delimiter: String): Seq[String] =
  val parts = Seq.newBuilder[String]
  var position = 0
  while position < input.length do
    val next = input.indexOf(delimiter, position)
    if next != -1 then
      if next > position then
        parts += input.substring(position, next)
      parts += delimiter
      position = next + delimiter.length
    else
      if position < input.length then
        parts += input.substring(position)
      position = input.length

  parts.result()


class BlockParser[T](
  parser: StreamParsing[T]
) extends StreamParsing[Iterator[T]]:
  override def parseFrom(input: ParseStream): Option[Iterator[T]] =
    Some(ChunkIterator.ofBlocks(input, parser))


class SeqParser[T](
  parser: StreamParsing[T],
  separator: String
) extends StreamParsing[IndexedSeq[T]]:
  override def parseFrom(input: ParseStream): Option[IndexedSeq[T]] =
    parser.parseFrom(input).map: first =>
      val list = IndexedSeq.newBuilder[T]
      list.addOne(first)
      var done = false
      while !done do
        input.tryParse: s =>
          if !s.tryConsume(separator) then
            done = true
            None
          else
            val next = parser.parseFrom(s)
            if next.isDefined then
              list.addOne(next.get)
              Some(true)
            else
              done = true
              None
      list.result()


class LazyLineParser[T](
  parser: StreamParsing[T]
) extends StreamParsing[Iterator[T]]:
  override def parseFrom(input: ParseStream): Option[Iterator[T]] =
    Some(ChunkIterator.ofLines(input, parser))


class HeaderParser[T](
  parser: StreamParsing[T]
) extends StreamParsing[T]:
  override def parseFrom(input: ParseStream): Option[T] =
    val header = parser.parseFrom(input)
    if header.isDefined && input.tryConsume("\n\n") then
      header
    else
      None


object RawLinesParser extends StreamParsing[Iterator[String]]:
  override def parseFrom(input: ParseStream): Option[Iterator[String]] =
    if input.hasNext then
      Some(ChunkIterator.ofLines(input, LineParser))
    else
      None

  private object LineParser extends StreamParsing[String]:
    override def parseFrom(input: ParseStream): Option[String] =
      if input.hasNext then
        Some(input.consumeWhile(_ != '\n'))
      else
        None


object WordParser extends StreamParsing[String]:
  override def parseFrom(input: ParseStream): Option[String] =
    val result = input.consumeWhile(c => (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))
    if result.nonEmpty then
      Some(result)
    else
      None
