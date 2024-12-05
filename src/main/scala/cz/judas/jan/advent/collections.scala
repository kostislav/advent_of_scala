package cz.judas.jan.advent

import scala.collection.mutable
import scala.collection.mutable.Builder


class AutoMap[K, V](valueFactory: K => V):
  private val values = mutable.HashMap[K, V]()

  def getOrCreate(key: K): V =
    values.get(key) match
      case Some(value) => value
      case None =>
        val value = valueFactory(key)
        values.put(key, value)
        value

  def put(key: K, value: V): Unit =
    values.put(key, value)

  def toMap: Map[K, V] =
    values.toMap


object AutoMap:
  def apply[K, V](valueFactory: K => V): AutoMap[K, V] =
    new AutoMap(valueFactory)

  def apply[K, V](valueFactory: => V): AutoMap[K, V] =
    new AutoMap(k => valueFactory)


class Histogram[K](values: Map[K, Int]):
  def get(key: K): Int =
    values.getOrElse(key, 0)


extension[A] (values: IterableOnce[A])
  def histogram: Histogram[A] =
    val result = AutoMap[A, Int](0)
    values.iterator.foreach(value => result.put(value, result.getOrCreate(value) + 1))
    Histogram(result.toMap)

  def getOnlyElement: A =
    val iterator = values.iterator
    val element = iterator.next()
    if iterator.hasNext then
      throw RuntimeException("There was mor than one element")
    else
      element


extension[A] (values: Seq[A])
  def filterByIndex(predicate: Int => Boolean): Seq[A] =
    values.zipWithIndex.filter((_, i) => predicate(i)).map(_._1)

  def cartesianProduct(onlyDifferent: Boolean): Seq[(A, A)] =
    values.flatMap: first =>
      values
        .filter(second => !onlyDifferent || second != first)
        .map(second => (first, second))


extension[A, B] (values: IterableOnce[(A, B)])
  def unzip: (Seq[A], Seq[B]) =
    val list = values.iterator.toIndexedSeq
    (
      list.map(_._1),
      list.map(_._2)
    )


extension[A, B] (value: (A, A))
  def mapAll(f: A => B): (B, B) =
    (f(value._1), f(value._2))


extension[A, B] (values: (Iterable[A], Iterable[B]))
  def zipElements: Iterable[(A, B)] =
    values._1.zip(values._2)


extension[K, V] (values: IterableOnce[(K, V)])
  def toMultiMap: Map[K, Seq[V]] =
    val result = AutoMap[K, mutable.Builder[V, Seq[V]]](Seq.newBuilder[V])
    values.iterator.foreach: (key, value) =>
      result.getOrCreate(key) += value
    result.toMap.view.mapValues(v => v.result()).toMap

  def toHashMap: mutable.HashMap[K, V] =
    val result = mutable.HashMap[K, V]()
    values.iterator.foreach(result.put)
    result

def absoluteDifference(x: Int, y: Int): Int =
  (x - y).abs
