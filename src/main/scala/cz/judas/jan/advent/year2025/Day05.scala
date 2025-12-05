package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, header, lines, pattern}

object Day05:
  def part1(input: InputData): Long =
    val (freshRanges, ids) = parseInput(input)

    ids
      .count(id => freshRanges.exists(_.contains(id)))

  def part2(input: InputData): Long =
    val (freshRanges, _) = parseInput(input)

    freshRanges.foldLeft(RangeSet.empty)(_ + _)
      .map(_.size)
      .sum

  private def parseInput(input: InputData): (Seq[Range2], Seq[Long]) =
    input.wholeAs[Seq[Range2] @lines @header, Seq[Long] @lines]


@pattern("{}-{}")
case class Range2(start: Long, end: Long):
  def contains(id: Long): Boolean =
    id >= start && id <= end

  def overlaps(other: Range2): Boolean =
    this.end >= other.start && other.end >= this.start

  def size: Long =
    end - start + 1


class RangeSet private(ranges: Set[Range2]) extends Iterable[Range2]:
  def +(range: Range2): RangeSet =
    val overlapping = ranges.filter(_.overlaps(range))
    val toMerge = overlapping + range
    RangeSet(ranges -- overlapping + Range2(toMerge.map(_.start).min, toMerge.map(_.end).max))

  override def iterator: Iterator[Range2] =
    ranges.iterator


object RangeSet:
  def empty: RangeSet = RangeSet(Set.empty)
