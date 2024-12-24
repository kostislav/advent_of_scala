package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, pattern, splitOnce, toMultiMap}

import scala.collection.mutable

object Day24:
  def part1(input: InputData): Long =
    val (wireString, gateString) =  input.whole.splitOnce("\n\n")
    val gates = gateString.linesIterator
      .flatMap: line =>
        val Array(input1, operationString, input2, _, output) = line.split(' ')
        val operation = operationString match
          case "AND" => Operation.And
          case "OR" => Operation.Or
          case "XOR" => Operation.Xor
        val gate = Gate(input1, operation, input2, output)
        Seq(input1 -> gate, input2 -> gate)
      .toMultiMap

    val wires = mutable.HashMap[String, Boolean]()
    val backlog = mutable.Queue[(String, Boolean)]()
    wireString.linesIterator
      .foreach: line =>
        val (wire, valueString) = line.splitOnce(": ")
        val value = valueString == "1"
        backlog.enqueue(wire -> value)

    while backlog.nonEmpty do
      val (wire, value) = backlog.dequeue()
      wires(wire) = value
      gates.getOrElse(wire, Seq.empty).foreach: gate =>
        if wires.contains(gate.input1) && wires.contains(gate.input2) then
          val input1 = wires(gate.input1)
          val input2 = wires(gate.input2)
          val output = gate.operation match
            case Operation.And => input1 & input2
            case Operation.Or => input1 | input2
            case Operation.Xor => input1 ^ input2
          backlog.enqueue(gate.output -> output)

    wires
      .iterator
      .filter((name, value) => name.startsWith("z"))
      .foldLeft(0L):
        case (current, (name, value)) =>
          if value then
            current | (1L << name.substring(1).toInt)
          else
            current

  def part2(input: InputData): Int =
    0


@pattern("{} {} {} -> {}")
case class Gate(input1: String, operation: Operation, input2: String, output: String)

enum Operation:
  @pattern("AND") case And
  @pattern("OR") case Or
  @pattern("XOR") case Xor
