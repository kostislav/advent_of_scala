package cz.judas.jan.advent

import scala.quoted.{Expr, Quotes}


inline def run(inline year: Int, inline day: Int, inline part: Int): (Any, Long) =
  ${ runImpl('year, 'day, 'part) }


private def runImpl(year: Expr[Int], day: Expr[Int], part: Expr[Int])(using q: Quotes): Expr[(Any, Long)] =
  import q.reflect.*
  val constYear = extractConstant(year)
  val constDay = extractConstant(day)
  val constPart = extractConstant(part)

  val call = Lambda(
    Symbol.spliceOwner,
    MethodType(List("input"))(_ => List(TypeRepr.of[InputData]), _ => TypeRepr.of[Any]), {
      case (_, List(inputExpr: Term)) =>
        Select.overloaded(
          Ref(Symbol.requiredModule(f"cz.judas.jan.advent.year${constYear}.Day$constDay%02d")),
          s"part${constPart}",
          List.empty,
          List(inputExpr)
        )
      case _ => throw RuntimeException("Oops")
    }
  )

  '{
    val input = InputData.real(${ year }, ${ day })
    runInternal(input, ${ call.asExprOf[InputData => Any] })
  }


private def extractConstant(value: Expr[Int])(using q: Quotes): Int =
  import q.reflect.*
  value.asTerm match
    case Inlined(_, _, Literal(IntConstant(constValue))) => constValue
    case _ => report.errorAndAbort("Expected constant value")


private def runInternal(input: InputData, puzzle: InputData => Any): (Any, Long) =
  val startTime = System.currentTimeMillis()
  val result = puzzle(input)
  val endTime = System.currentTimeMillis()
  (result, endTime - startTime)
