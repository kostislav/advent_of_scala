package cz.judas.jan.advent

import scala.quoted.{Expr, Quotes}


inline def run(inline year: Int, inline day: Int, inline part: Int): Any =
  ${ runImpl('year, 'day, 'part) }

def runImpl(year: Expr[Int], day: Expr[Int], part: Expr[Int])(using q: Quotes): Expr[Any] =
  import q.reflect.*
  val constYear = extractConstant(year)
  val constDay = extractConstant(day)
  val constPart = extractConstant(part)

  Select.overloaded(
    Ref(Symbol.requiredModule(f"cz.judas.jan.advent.year${constYear}.Day$constDay%02d")),
    s"part${constPart}",
    List.empty,
    List('{ InputData.real(${year}, ${day}) }.asTerm)
  ).asExprOf[Any]

def extractConstant(value: Expr[Int])(using q: Quotes): Int =
  import q.reflect.*
  value.asTerm match
    case Inlined(_, _, Literal(IntConstant(constValue))) => constValue
    case _ => report.errorAndAbort("Expected constant value")
