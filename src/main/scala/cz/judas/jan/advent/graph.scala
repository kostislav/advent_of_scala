package cz.judas.jan.advent

import scala.collection.mutable


def linearizeDag[T](graph: Map[T, Iterable[T]]): Seq[T] =
  val remaining = graph
    .view.mapValues(neighbors => mutable.HashSet.newBuilder[T].addAll(neighbors).result())
    .toHashMap
  val result = mutable.ArrayBuffer[T]()

  while remaining.nonEmpty do
    val next = remaining.filter((key, values) => values.isEmpty).iterator.next()._1
    result += next
    remaining.remove(next)
    remaining.values.foreach(neighbors => neighbors.remove(next))

  result.toSeq


def linearizeDag[T](first: T, graph: Map[T, Iterable[T]]): Seq[T] =
  linearizeDag(graph ++ Map(first -> Seq.empty))
