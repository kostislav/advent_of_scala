package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.year2024.DiskContents.{EmptySpace, FileBlock}
import cz.judas.jan.advent.{InputData, minByOptionIfDefined, repeat}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day09:
  def part1(input: InputData): Long =
    val perBlockMap = parse(input)
      .flatMap:
        case FileBlock(length, id) => repeat(length, FileBlock(1, id))
        case EmptySpace(length) => repeat(length, EmptySpace(1))

    solve(perBlockMap)

  def part2(input: InputData): Long =
    solve(parse(input))

  private def parse(input: InputData): Iterator[DiskContents] =
    input
      .whole
      .map(c => c - '0')
      .grouped(2)
      .zipWithIndex
      .flatMap: (chunk, id) =>
        Seq(FileBlock(chunk(0), id), EmptySpace(chunk.lift(1).getOrElse(0)))

  private def solve(diskMap: Iterator[DiskContents]): Long =
    val blocks = ArrayBuffer[EnhancedFileBlock]()
    val emptySpaces = Vector.tabulate(10)(_ => mutable.PriorityQueue[Int]()(Ordering[Int].reverse))

    var position = 0
    diskMap.foreach:
      case block: FileBlock if block.length > 0 =>
        blocks.addOne(EnhancedFileBlock(position, block.length, block.id))
        position += block.length
      case EmptySpace(length) if length > 0 =>
        emptySpaces(length).enqueue(position)
        position += length
      case _ =>

    val reorderedBlocks = blocks.reverseIterator
      .map: block =>
        (block.length to 9).minByOptionIfDefined(length => emptySpaces(length).headOption.filter(_ < block.position)) match
          case Some(emptyLength) =>
            val emptyPosition = emptySpaces(emptyLength).dequeue()
            if emptyLength > block.length then
              emptySpaces(emptyLength - block.length).enqueue(emptyPosition + block.length)
            block.copy(position = emptyPosition)
          case _ => block

    reorderedBlocks.map(_.score).sum


enum DiskContents:
  case FileBlock(length: Int, id: Int)
  case EmptySpace(length: Int)


case class EnhancedFileBlock(position: Int, length: Int, id: Int):
  def score: Long =
    (position * length + ((length - 1) * length / 2)) * id.toLong

