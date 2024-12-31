package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, getOnlyElement, headerWith, linesOf, pattern, toMultiMap, word}

import scala.collection.mutable

object Day24:
  def part1(input: InputData): Long =
    val (wires, gateDefs) = input.parseStructured(
      headerWith(linesOf[(String @word, Int) @pattern("{}: {}")]),
      linesOf[Gate],
    )
    val gates = gateDefs
      .flatMap(gate => Seq(gate.input1 -> gate, gate.input2 -> gate))
      .toMultiMap

    val wireValues = mutable.HashMap[String, Boolean]()
    val backlog = mutable.Queue[(String, Boolean)]()
    wires.foreach: (wire, value) =>
      backlog.enqueue(wire -> (value == 1))

    while backlog.nonEmpty do
      val (wire, value) = backlog.dequeue()
      wireValues(wire) = value
      gates.getOrElse(wire, Seq.empty).foreach: gate =>
        if wireValues.contains(gate.input1) && wireValues.contains(gate.input2) then
          val input1 = wireValues(gate.input1)
          val input2 = wireValues(gate.input2)
          val output = gate.operation match
            case Operation.And => input1 & input2
            case Operation.Or => input1 | input2
            case Operation.Xor => input1 ^ input2
          backlog.enqueue(gate.output -> output)

    wireValues
      .iterator
      .filter((name, value) => name.startsWith("z"))
      .foldLeft(0L):
        case (current, (name, value)) =>
          if value then
            current | (1L << name.substring(1).toInt)
          else
            current

  def part2(input: InputData): String =
    val (wires, gateDefs) = input.parseStructured(
      headerWith(linesOf[(String @word, Int) @pattern("{}: {}")]),
      linesOf[Gate],
    )
    val gates = GateSet(
      mutable.HashSet.from(
        gateDefs
          .map(gate => SimpleGate(Set(gate.input1, gate.input2), gate.operation, gate.output))
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
case class Gate(input1: String @word, operation: Operation, input2: String @word, output: String @word)

enum Operation:
  @pattern("AND") case And
  @pattern("OR") case Or
  @pattern("XOR") case Xor
