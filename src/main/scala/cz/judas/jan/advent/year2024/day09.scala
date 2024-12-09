package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.year2024.DiskContents.{FileBlock, EmptySpace}
import cz.judas.jan.advent.InputData

object Day09:
  def part1(input: InputData): Long =
    val diskMap = (input.whole + (if input.whole.length % 2 == 0 then "" else "0"))
      .grouped(2)
      .zipWithIndex
      .flatMap: (chunk, id) =>
        (0 until (chunk(0) - '0')).map(_ => FileBlock(id))
         ++ (0 until (chunk(1) - '0')).map(_ => EmptySpace)
      .toIndexedSeq

    var head = 0
    var tail = diskMap.size - 1
    var sum = 0L
    while head <= tail do
      diskMap(head) match
        case FileBlock(id) =>
          sum += head * id
        case EmptySpace =>
          while tail > head && diskMap(tail) == EmptySpace do
            tail -= 1
          if tail > head then
            sum += head * diskMap(tail).asInstanceOf[FileBlock].id
            tail -= 1
      head += 1
    sum

  def part2(input: InputData): Long =
    val diskMap = (input.whole + (if input.whole.length % 2 == 0 then "" else "0"))
      .grouped(2)
          .zipWithIndex
          .flatMap: (chunk, id) =>
            (0 until (chunk(0) - '0')).map(_ => FileBlock(id))
              ++ (0 until (chunk(1) - '0')).map(_ => EmptySpace)
      .toArray

    var tail = diskMap.length - 1
    while tail >= 0 do
      while tail >= 0 && diskMap(tail) == EmptySpace do
        tail -= 1
      if tail >= 0 then
        val endPos = tail
        val id = diskMap(tail).asInstanceOf[FileBlock].id
        while tail >= 0 && diskMap(tail) == FileBlock(id) do
          tail -= 1
        val startPos = tail + 1
        val length = endPos - startPos + 1
        var head = 0
        var spaceSize = 0
        while head < startPos do
          diskMap(head) match
            case FileBlock(_) =>
              spaceSize = 0
            case EmptySpace =>
              spaceSize += 1
              if spaceSize == length then
                (0 until length).foreach: i =>
                  diskMap(head - i) = diskMap(endPos - i)
                  diskMap(endPos - i) = EmptySpace
                head = startPos
            head += 1

    diskMap
      .zipWithIndex
      .map:
        case (FileBlock(id), i) => i.toLong * id
        case _ => 0L
      .sum


enum DiskContents:
  case FileBlock(id: Int)
  case EmptySpace

