package cz.judas.jan.advent

import scala.collection.mutable


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


extension[A] (values: Seq[A])
  def filterByIndex(predicate: Int => Boolean): Seq[A] =
    values.zipWithIndex.filter((_, i) => predicate(i)).map(_._1)


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


def absoluteDifference(x: Int, y: Int): Int =
  (x - y).abs
