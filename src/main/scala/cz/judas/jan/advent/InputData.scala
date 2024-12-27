package cz.judas.jan.advent

import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}

class pattern(val shape: String) extends StaticAnnotation

class separatedBy(val separator: String) extends StaticAnnotation

class word extends StaticAnnotation

class InputData(content: String):
  def lines: Iterator[String] =
    content.linesIterator

  def whole: String =
    content

  def asArray2d: Array2d =
    Array2d.fromRows(lines.toIndexedSeq)

  inline def linesAs[T]: Iterator[T] =
    ChunkIterator.ofLines(ParseStream(whole), createParser[T])

  inline def wholeAs[T]: T =
    parseStructured(createParser[T])

  def parseStructured[T](parser: StreamParsing[T]): T =
    parser.parseFrom(ParseStream(whole)).get

  def parseStructured[A, B](headerParser: StreamParsing[A], restParser: StreamParsing[B]): (A, B) =
    val stream = ParseStream(whole)
    val header = headerParser.parseFrom(stream).get
    val rest = restParser.parseFrom(stream).get
    (header, rest)

  def linesAs[A, B](separatedBy: String)(using aParser: StreamParsing[A], bParser: StreamParsing[B]): Iterator[(A, B)] =
    // TODO generate using macro
    val parser = new StreamParsing[(A, B)]:
      override def parseFrom(input: ParseStream): Option[(A, B)] =
        val first = aParser.parseFrom(input)
        if first.isDefined then
          if input.tryConsume(separatedBy) then
            val second = bParser.parseFrom(input)
            if second.isDefined then
              Some((first.get, second.get))
            else
              None
          else
            None
        else
          None

    ChunkIterator.ofLines(ParseStream(content), parser)


inline def headerOf[T]: StreamParsing[T] =
  HeaderParser(createParser[T])

inline def blocksOf[T]: StreamParsing[Iterator[T]] =
  BlockParser(createParser[T])

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
    tpe.tpe.asTypeRepr.asType match
      case '[tT] =>
        if !parseExprs.contains(tpe) then
          if tpe == TypeRef.unannotated[Int] then
            parseExprs.put(tpe, Select.unique('{ intParser }.asTerm, "parseFrom"))
          else if tpe == TypeRef.unannotated[Long] then
            parseExprs.put(tpe, Select.unique('{ longParser }.asTerm, "parseFrom"))
          else if tpe == TypeRef(SimpleType.of[String], List(Annotation("word", List.empty))) then
            parseExprs.put(tpe, Select.unique('{WordParser}.asTerm, "parseFrom"))
          else
            val methodSymbol = Symbol.newMethod(
              Symbol.spliceOwner,
              s"f${parseMethods.size}",
              MethodType(List("input"))(
                _ => List(TypeRepr.of[ParseStream]),
                _ => TypeRepr.of[Option[tT]]
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
                          itemType.tpe.asTypeRepr.asType match
                            case '[itemTypeT] =>
                              '{
                                val first = ${Apply(itemParser, List(inputTerm)).asExprOf[Option[itemTypeT]]}
                                if first.isDefined then
                                  val list = IndexedSeq.newBuilder[itemTypeT]
                                  list.addOne(first.get)
                                  var done = false
                                  while !done do
                                    if !${input.asExprOf[ParseStream]}.tryConsume(${Expr(separatedBy)}) then
                                      done = true
                                    else
                                      val next = ${Apply(itemParser, List(inputTerm)).asExprOf[Option[itemTypeT]]}
                                      if next.isDefined then
                                        list.addOne(next.get)
                                      else
                                        done = true
                                  Some(list.result())
                                else
                                  None
                              }.asTerm.changeOwner(methodSymbol)
                        else
                          val maybePattern = (tpe.annotations ++ classType.annotations).find(_.name == "pattern").map(_.parameters.head)
                          val constructor = classType.constructor
                          maybePattern match
                            case Some(pattern) =>
                              caseClassParserBody(
                                constructor.parameters.iterator,
                                splitAndKeepDelimiters(pattern, "{}").iterator,
                                List.empty,
                                constructor,
                                inputTerm,
                                methodSymbol,
                              )
                            case None =>
                              if constructor.parameters.size == 1 then
                                val parameter = constructor.parameters.head
                                parameter.tpe.tpe.asTypeRepr.asType match
                                  case '[parameterT] =>
                                    val variable = Symbol.newVal(methodSymbol, "v", TypeRepr.of[Option[parameterT]], Flags.EmptyFlags, Symbol.noSymbol)
                                    Block(
                                      List(
                                        ValDef(variable, Some(Apply(getOrCreateParser(parameter.tpe), List(inputTerm)).changeOwner(variable))),
                                      ),
                                      If(
                                        Select.unique(Ref(variable), "isDefined"),
                                        some(constructor.call(List(Select.unique(Ref(variable), "get"))), tpe.tpe),
                                        '{ None }.asTerm
                                      )
                                    )
                              else
                                report.errorAndAbort(s"Classes with more than one parameter need a @pattern annotation")
                      case unionType: UnionType =>
                        unionType.options
                          .reverse
                          .foldLeft('{ None }.asTerm):
                            case (rest, child) =>
                              val variable = Symbol.newVal(methodSymbol, "v", TypeRepr.of[Option[tT]], Flags.EmptyFlags, Symbol.noSymbol)
                              Block(
                                List(
                                  ValDef(variable, Some(Apply(getOrCreateParser(child), List(inputTerm)).changeOwner(variable))),
                                ),
                                If(
                                  Select.unique(Ref(variable), "isDefined"),
                                  some(Select.unique(Ref(variable), "get"), tpe.tpe),
                                  rest
                                )
                              )
                      case ObjectType(instance) =>
                        If(
                          Apply(Select.unique(inputTerm, "tryConsume"), List(Literal(StringConstant(instance.name.toLowerCase)))),
                          some(Ref(instance), tpe.tpe),
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
        parameter.tpe.tpe.asTypeRepr.asType match
          case '[fieldT] =>
            val variable = Symbol.newVal(enclosingMethod, s"v${variables.size}", TypeRepr.of[Option[fieldT]], Flags.EmptyFlags, Symbol.noSymbol)
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

  private def some(value: Term, tpe: SimpleType): Term =
    tpe.asTypeRepr.asType match
      case '[t] => '{ Some(${ value.asExprOf[t] }) }.asTerm

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

    override def toString: String =
      typeRepr.toString

  private class UnionType(private val original: TypeRepr, val options: List[TypeRef]) extends SimpleType:
    def asTypeRepr: TypeRepr =
      original

  private case class ObjectType(instance: Symbol) extends SimpleType:
    def asTypeRepr: TypeRepr =
      instance.typeRef

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
    val content = Path(f"input/year$year/day${day}%02d").readString()
    InputData(content)

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
    position + n < input.length

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
    if input.peek == '-' then
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


private def splitAndKeepDelimiters(input: String, delimiter: String): Seq[String] =
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
      if position < input.length - 1 then
        parts += input.substring(position)
      position = input.length

  parts.result()


class BlockParser[T](
  parser: StreamParsing[T]
) extends StreamParsing[Iterator[T]]:
  override def parseFrom(input: ParseStream): Option[Iterator[T]] =
    Some(ChunkIterator.ofBlocks(input, parser))


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
