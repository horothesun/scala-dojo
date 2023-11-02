package adventofcode22

import cats.data.NonEmptyList
import cats.implicits._
import Day5.CrateItem._
import ListOps._

object Day5 {

  case class Stack[A] private (as: List[A]) {
    def push(a: A): Stack[A] = Stack(a :: as)
    def pop: Option[(A, Stack[A])] = as match {
      case h :: t => Some((h, Stack(t)))
      case Nil    => None
    }
    def peek: Option[A] = as.headOption
  }
  object Stack {
    def apply[A](as: A*): Stack[A] = Stack.apply[A](as.toList)
    def empty[A]: Stack[A] = Stack(List.empty)
  }

  sealed trait CrateItem
  object CrateItem {
    case class NoCrate() extends CrateItem
    object NoCrate {
      def show: String = "[~]"
      def from(s: String): Option[NoCrate] = if (s == show) Some(NoCrate()) else None
    }

    case class Crate(c: Char) extends CrateItem
    object Crate {
      def from(s: String): Option[Crate] =
        s.toList match {
          case List('[', c, ']') => Some(Crate(c))
          case _                 => None
        }
    }

    def from(s: String): Option[CrateItem] = NoCrate.from(s).orElse(Crate.from(s))
  }

  case class Stacks(value: NonEmptyList[Stack[Crate]]) {
    def peekAll: NonEmptyList[Option[Crate]] = value.map(_.peek)
  }
  object Stacks {

    def apply(s1: Stack[Crate], ss: Stack[Crate]*): Stacks = Stacks(NonEmptyList(s1, ss.toList))

    def empty(n: Int): Option[Stacks] = List.fill(n)(Stack.empty[Crate]).toNel.map(Stacks.apply)

    def from(filledCrateStacks: List[String]): Option[Stacks] =
      filledCrateStacks.reverse match {
        case footer :: Nil => Stacks.empty(n = footer.count(_ != ' '))
        case footer :: bottomRow :: otherRows =>
          (bottomRow :: otherRows)
            .traverse(_.split(' ').toList.toNel.flatMap(_.traverse(CrateItem.from)))
            .flatMap { crateItemsRows =>
              Stacks.empty(n = bottomRow.length).map { emptyStacks =>
                crateItemsRows.foldLeft[NonEmptyList[Stack[Crate]]](emptyStacks.value) { case (ss, row) =>
                  ss.zipWith(row) { case (s, ci) =>
                    ci match {
                      case NoCrate()    => s
                      case c @ Crate(_) => s.push(c)
                    }
                  }
                }
              }
            }
            .map(Stacks.apply)
        case Nil => None
      }

  }

  case class Move(n: Int, from: Int, to: Int) {
    def performOn(stacks: Stacks): Option[Stacks] =
      for {
        extendedStacks <- Some(Stacks(stacks.value :+ Stack.empty[Crate]))
        tmpStackIndex = 1 + stacks.value.length
        baseMovePlan = BaseMovePlan(
          List.fill(n)(BaseMove(from, to = tmpStackIndex)) ++
            List.fill(n)(BaseMove(from = tmpStackIndex, to))
        )
        newExtendedStacks = baseMovePlan.performOn(extendedStacks)
        newStacks <- newExtendedStacks.value.toList.dropRight(1).toNel.map(Stacks.apply)
      } yield newStacks
  }
  object Move {
    def from(s: String): Option[Move] =
      s.split(' ') match {
        case Array("move", n, "from", from, "to", to) =>
          (
            n.toIntOption,
            from.toIntOption,
            to.toIntOption
          ).flatMapN { case (n, from, to) =>
            val validRange = 1 to 9
            if (n > 0 && validRange.contains(from) && validRange.contains(to)) Some(Move(n, from, to))
            else None
          }
        case _ => None
      }
  }

  case class BaseMove(from: Int, to: Int) {
    def performOn(stacks: Stacks): Option[Stacks] = for {
      (from0Based, to0Based) <- Some((from - 1, to - 1))
      fromStack <- stacks.value.get(from0Based)
      (c, newFromStack) <- fromStack.pop
      toStack <- stacks.value.get(to0Based)
      newToStack = toStack.push(c)
      stackListWithNewFrom <- updatedOption(from0Based, newFromStack)(stacks.value.toList)
      newStackList <- updatedOption(to0Based, newToStack)(stackListWithNewFrom)
      newStackNel <- newStackList.toNel
    } yield Stacks(newStackNel)
  }

  case class BaseMovePlan(baseMoves: List[BaseMove]) {
    // if a BaseMove can't be performed, just continue folding on the same Stacks
    def performOn(stacks: Stacks): Stacks =
      baseMoves.foldLeft(stacks) { case (ss, bm) => bm.performOn(ss).getOrElse(ss) }
  }
  object BaseMovePlan {

    def apply(baseMoves: BaseMove*): BaseMovePlan = BaseMovePlan.apply(baseMoves.toList)

    def from(movePlan: MovePlan): BaseMovePlan =
      BaseMovePlan(movePlan.moves.flatMap(m => List.fill(m.n)(BaseMove(from = m.from, to = m.to))))

  }

  case class MovePlan(moves: List[Move]) {
    // if a Move can't be performed, just continue folding on the same Stacks
    def performOn(stacks: Stacks): Stacks = moves.foldLeft(stacks) { case (ss, m) => m.performOn(ss).getOrElse(ss) }
  }
  object MovePlan {

    def apply(moves: Move*): MovePlan = MovePlan.apply(moves.toList)

    def from(input: List[String]): Option[MovePlan] = input.traverse(Move.from).map(ms => MovePlan(ms))

  }

  def getFilledCrateStacksInput(crateStacks: List[String]): List[String] =
    crateStacks.map(s => s" $s".replaceAll(s" {4}", s" ${NoCrate.show}").drop(1))

  def getCrateStacksAndMovesInputSections(input: List[String]): Option[(List[String], List[String])] =
    splitListBy("")(input) match {
      case crateStacks :: moves :: Nil => Some(crateStacks, moves)
      case _                           => None
    }

  def getStacksAndMovePlan(input: List[String]): Option[(Stacks, MovePlan)] =
    getCrateStacksAndMovesInputSections(input).flatMap { case (crateStacks, moves) =>
      (
        Stacks.from(getFilledCrateStacksInput(crateStacks)),
        MovePlan.from(moves)
      ).tupled
    }

  def showTopCrates(topCrates: NonEmptyList[Option[Crate]]): String =
    topCrates.map(_.fold(ifEmpty = " ")(c => s"${c.c}")).mkString_("")

  def getStacksTopCratesAfterPerformingBaseMovePlan(input: List[String]): Option[String] =
    getStacksAndMovePlan(input).map { case (stacks, movePlan) =>
      val topCrates = BaseMovePlan.from(movePlan).performOn(stacks).peekAll
      showTopCrates(topCrates)
    }

  def getStacksTopCratesAfterPerformingMovePlan(input: List[String]): Option[String] =
    getStacksAndMovePlan(input).map { case (stacks, movePlan) =>
      val topCrates = movePlan.performOn(stacks).peekAll
      showTopCrates(topCrates)
    }

}
