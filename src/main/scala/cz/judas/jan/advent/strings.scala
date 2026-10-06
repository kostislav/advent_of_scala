package cz.judas.jan.advent

extension (value: String)
  def maybeIndexOf(str: String): Option[Int] =
    val index = value.indexOf(str)
    if index == -1 then None else Some(index)