package adventofcode22

import cats.syntax.all._
import Day10.Crt._
import Day10.Instruction._
import Day10.Pixel._

object Day10 {

  case class SignalStrength(value: Int)
  object SignalStrength {
    implicit val numeric: Numeric[SignalStrength] = Numeric[Int].imap(SignalStrength.apply)(_.value)
  }

  case class Registers(x: Int)
  object Registers {
    def initial: Registers = Registers(x = 1)
  }

  case class NumberOfCycles(value: Int)

  sealed trait Instruction {
    def getRequiredCycles: NumberOfCycles = this match {
      case NoOp    => NumberOfCycles(1)
      case AddX(_) => NumberOfCycles(2)
    }
  }
  object Instruction {
    case object NoOp extends Instruction {
      def from(s: String): Option[Instruction] = s match {
        case "noop" => Some(NoOp)
        case _      => None
      }
    }
    case class AddX(n: Int) extends Instruction
    object AddX {
      def from(s: String): Option[Instruction] = s.split(" ") match {
        case Array("addx", n) => n.toIntOption.map(AddX.apply)
        case _                => None
      }
    }

    def from(s: String): Option[Instruction] = NoOp.from(s).orElse(AddX.from(s))
  }

  def getNewRegisters(start: Registers, ins: Instruction): Registers = ins match {
    case NoOp    => start
    case AddX(n) => Registers(x = start.x + n)
  }

  def getSteps(start: Registers, ins: Instruction): List[Registers] = List.fill(ins.getRequiredCycles.value)(start)

  def getRegistersByCycleWithStart(start: Registers, instructions: List[Instruction]): List[Registers] =
    start :: getRegistersByCycle(start, instructions)

  def getRegistersByCycle(start: Registers, instructions: List[Instruction]): List[Registers] =
    instructions
      .foldLeft[(List[Registers], Registers)]((List.empty, start)) { case ((acc, prevRegister), ins) =>
        (
          acc ++ getSteps(prevRegister, ins),
          getNewRegisters(prevRegister, ins)
        )
      }
      ._1

  def isCycleOfInterest(index: Int): Boolean = Array(20, 60, 100, 140, 180, 220).contains(index)

  def getSignalStrength(index: Int, registers: Registers): SignalStrength = SignalStrength(index * registers.x)

  def getInstructions(input: List[String]): Option[List[Instruction]] = input.traverse(Instruction.from)

  def getSignalStrengthAtCyclesOfInterestSum(instructions: List[Instruction]): SignalStrength =
    getRegistersByCycleWithStart(Registers.initial, instructions).zipWithIndex.filter { case (_, index) =>
      isCycleOfInterest(index)
    }.map { case (r, index) => getSignalStrength(index, r) }.sum

  def getSignalStrengthAtCyclesOfInterestSum(input: List[String]): Option[SignalStrength] =
    getInstructions(input).map(getSignalStrengthAtCyclesOfInterestSum)

  sealed trait Pixel {
    def encoded: Char = this match {
      case On  => '#'
      case Off => '.'
    }
  }
  object Pixel {
    case object On extends Pixel
    case object Off extends Pixel
  }

  case class Crt(rows: Array[Array[Pixel]]) {

    def encoded: String = rows.map(_.map(_.encoded).mkString).mkString("\n")

    def updated(c: NumberOfCycles, p: Pixel): Crt = {
      val (row, col) = getPixelRowAndColumn(c)
      Crt(rows = rows.updated(row, rows(row).updated(col, p)))
    }

  }
  object Crt {

    val WIDTH: Int = 40
    val HEIGHT: Int = 6

    def allOffRow: Array[Pixel] = Array.fill[Pixel](WIDTH)(Off)

    def allOff: Crt = Crt(rows = Array.fill[Array[Pixel]](HEIGHT)(allOffRow))

    def getPixelRowAndColumn(c: NumberOfCycles): (Int, Int) = {
      val c_mod = (c.value - 1) % (WIDTH * HEIGHT)
      val row = c_mod / WIDTH
      val col = c_mod - (row * WIDTH)
      (row, col)
    }

  }

  def getSpriteRow(spriteMidX0Based: Int): Array[Pixel] = {
    val visibleXs = ((spriteMidX0Based - 1) to (spriteMidX0Based + 1)).filter(x => 0 <= x && x < Crt.WIDTH)
    visibleXs.foldLeft[Array[Pixel]](Crt.allOffRow) { case (row, i) => row.updated(i, On) }
  }

  def getFinalCrt(instructions: List[Instruction]): Crt =
    getRegistersByCycleWithStart(Registers.initial, instructions).zipWithIndex
      .drop(1)
      .map { case (rs, index) => (rs, NumberOfCycles(index)) }
      .foldLeft[Crt](Crt.allOff) { case (prevCrt, (rs, c)) =>
        val (_, newPixelCol) = Crt.getPixelRowAndColumn(c)
        val spriteRow = getSpriteRow(spriteMidX0Based = rs.x)
        val newPixel = spriteRow(newPixelCol)
        prevCrt.updated(c, newPixel)
      }

  def getFinalCrt(input: List[String]): Option[Crt] = getInstructions(input).map(getFinalCrt)

}
