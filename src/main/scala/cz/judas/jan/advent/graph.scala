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


def shortestPath[T](start: T)(isTarget: T => Boolean)(edges: T => IterableOnce[(T, Int)]): Option[Int] =
  val toVisit = mutable.PriorityQueue[(T, Int)]()(Ordering.by[(T, Int), Int](_._2).reverse)
  val dist = mutable.HashMap[T, Int]()
  var result: Option[Int] = None
  toVisit.addOne((start, 0))
  dist.put(start, 0)
  while toVisit.nonEmpty && result.isEmpty do
    val (closest, distance) = toVisit.dequeue()
    if isTarget(closest) then
      result = Some(distance)
    else if dist(closest) == distance then
      edges(closest).iterator.foreach: (neighbor, weight) =>
        val neighborWeight = distance + weight
        if dist.get(neighbor).forall(neighborDistance => neighborDistance > neighborWeight) then
          toVisit.addOne((neighbor, neighborWeight))
          dist.put(neighbor, neighborWeight)

  result
