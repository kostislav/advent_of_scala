package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, getOnlyElement, pattern, splitOnce, toMultiMap}

import scala.collection.mutable

object Day24:
  def part1(input: InputData): Long =
    val (wireString, gateString) = input.whole.splitOnce("\n\n")
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

  def part2(input: InputData): String =
    val (wireString, gateString) = input.whole.splitOnce("\n\n")
    val gates = GateSet(
      mutable.HashSet.from(
        gateString.linesIterator
          .map: line =>
            val Array(input1, operationString, input2, _, output) = line.split(' ')
            val operation = operationString match
              case "AND" => Operation.And
              case "OR" => Operation.Or
              case "XOR" => Operation.Xor
            SimpleGate(Set(input1, input2), operation, output)
      )
    )

    val maxBit = gates.maxBit

    if gates.getAndRemove("x00", "y00", Operation.Xor) != "z00" then
      throw RuntimeException("00")
    var carry = gates.getAndRemove("x00", "y00", Operation.And)

    (1 to maxBit).foreach: i =>
      val x = f"x${i}%02d"
      val y = f"y${i}%02d"
      val z = f"z${i}%02d"
      val hs = gates.getAndRemove(x, y, Operation.Xor)
      val fc = gates.getAndRemove(x, y, Operation.And)
      val outputGate = gates.getAndRemove(carry, hs, Operation.Xor)
      if outputGate != z then
        gates.swap(z, outputGate)
      val sc = gates.getAndRemove(carry, hs, Operation.And)
      carry = gates.getAndRemove(fc, sc, Operation.Or)

    gates.swaps.toSeq.sorted.mkString(",")


class GateSet(gates: mutable.Set[SimpleGate]):
  private val swapsMut = mutable.HashSet[String]()

  def getAndRemove(input1: String, input2: String, operation: Operation): String =
    val maybeGate = gates.find(gate => gate.inputs == Set(input1, input2) && gate.operation == operation)
    if maybeGate.isEmpty then
      val actualGate = gates.filter(gate => (gate.inputs.contains(input1) || gate.inputs.contains(input2)) && gate.operation == operation).getOnlyElement
      swapsMut ++= actualGate.inputs.diff(Set(input1, input2))
      gates.remove(actualGate)
      actualGate.output
    else
      val gate = maybeGate.get
      gates.remove(gate)
      gate.output

  def swap(wire1: String, wire2: String): Unit =
    swapsMut += wire1
    swapsMut += wire2
    gates.filter(_.output == wire1).foreach: toReplace =>
      gates.remove(toReplace)
      gates.addOne(toReplace.copy(output = wire2))

  def swaps: Set[String] =
    swapsMut.toSet

  def maxBit: Int =
    gates
      .map(_.output)
      .filter(_.startsWith("z"))
      .map(_.substring(1).toInt)
      .max - 1


case class SimpleGate(inputs: Set[String], operation: Operation, output: String)

@pattern("{} {} {} -> {}")
case class Gate(input1: String, operation: Operation, input2: String, output: String)

enum Operation:
  @pattern("AND") case And
  @pattern("OR") case Or
  @pattern("XOR") case Xor
