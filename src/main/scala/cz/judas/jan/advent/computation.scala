package cz.judas.jan.advent

import scala.collection.mutable


private def recurseMemoized[A, B](start: A)(recursion: (A, A => B) => B): B =
  val memory = mutable.HashMap[A, B]()

  def recurse(current: A): B =
    memory.get(current) match
      case Some(value) => value
      case None =>
        val value = recursion(current, recurse)
        memory.put(current, value)
        value

  recursion(start, recurse)