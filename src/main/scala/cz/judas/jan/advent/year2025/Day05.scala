package cz.judas.jan.advent.year2025

import com.google.common.collect.TreeRangeSet
import cz.judas.jan.advent.{InputData, header, lines, pattern}

import scala.jdk.CollectionConverters.{IterableHasAsScala, SeqHasAsJava}

object Day05:
  def part1(input: InputData): Long =
    val (freshRanges, ids) = input.wholeAs[Seq[Range2] @lines @header, Seq[Long] @lines]

    ids
      .count(id => freshRanges.exists(_.contains(id)))

  def part2(input: InputData): Long =
    val (freshRanges, _) = input.wholeAs[Seq[Range2] @lines @header, Seq[String] @lines]

    val xex = TreeRangeSet.create(freshRanges.map(range => com.google.common.collect.Range.closed(range.start, range.end)).asJava)
    xex.asRanges().asScala.map(range => range.upperEndpoint() - range.lowerEndpoint() + 1).sum


@pattern("{}-{}")
case class Range2(start: Long, end: Long):
  def contains(id: Long): Boolean =
    id >= start && id <= end