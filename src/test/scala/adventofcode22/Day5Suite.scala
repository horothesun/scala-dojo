package adventofcode22

import munit.ScalaCheckSuite
import Day5._
import Day5.CrateItem._
import Day5Suite._

class Day5Suite extends ScalaCheckSuite {

  test("getFilledCrateStacksInput (small input A)") {
    val input = List(
      "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      getFilledCrateStacksInput(input),
      List(
        "[~] [D] [~]",
        "[N] [C] [~]",
        "[Z] [M] [P]",
        " 1   2   3 "
      )
    )
  }

  test("getFilledCrateStacksInput (small input B)") {
    val input = List(
      "[D]        ",
      "[N] [C]    ",
      "[Z] [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      getFilledCrateStacksInput(input),
      List(
        "[D] [~] [~]",
        "[N] [C] [~]",
        "[Z] [M] [P]",
        " 1   2   3 "
      )
    )
  }

  test("getFilledCrateStacksInput (small input C)") {
    val input = List(
      "        [Z]",
      "        [N]",
      "    [C] [D]",
      "    [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      getFilledCrateStacksInput(input),
      List(
        "[~] [~] [Z]",
        "[~] [~] [N]",
        "[~] [C] [D]",
        "[~] [M] [P]",
        " 1   2   3 "
      )
    )
  }

  test("getFilledCrateStacksInput (small input D)") {
    val input = List(
      "        [Z]",
      "        [N]",
      "[M]     [D]",
      "[C]     [P]",
      " 1   2   3 "
    )
    assertEquals(
      getFilledCrateStacksInput(input),
      List(
        "[~] [~] [Z]",
        "[~] [~] [N]",
        "[M] [~] [D]",
        "[C] [~] [P]",
        " 1   2   3 "
      )
    )
  }

  test("getFilledCrateStacksInput (small input E)") {
    val input = List(
      "        [Z]",
      "        [N]",
      "        [D]",
      "[C] [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      getFilledCrateStacksInput(input),
      List(
        "[~] [~] [Z]",
        "[~] [~] [N]",
        "[~] [~] [D]",
        "[C] [M] [P]",
        " 1   2   3 "
      )
    )
  }

  test("Stacks.from (small input A)") {
    val filledCrateStacks = List(
      "[~] [D] [~]",
      "[N] [C] [~]",
      "[Z] [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      Stacks.from(filledCrateStacks),
      Some(
        Stacks(
          Stack.of(Crate('Z')).push(Crate('N')),
          Stack.of(Crate('M')).push(Crate('C')).push(Crate('D')),
          Stack.of(Crate('P'))
        )
      )
    )
  }

  test("Stacks.from (small input B)") {
    val filledCrateStacks = List(
      "[D] [~] [~]",
      "[N] [C] [~]",
      "[Z] [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      Stacks.from(filledCrateStacks),
      Some(
        Stacks(
          Stack.of(Crate('Z')).push(Crate('N')).push(Crate('D')),
          Stack.of(Crate('M')).push(Crate('C')),
          Stack.of(Crate('P'))
        )
      )
    )
  }

  test("Stacks.from (small input C)") {
    val filledCrateStacks = List(
      "[~] [~] [Z]",
      "[~] [~] [N]",
      "[~] [C] [D]",
      "[~] [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      Stacks.from(filledCrateStacks),
      Some(
        Stacks(
          Stack.empty[Crate],
          Stack.of(Crate('M')).push(Crate('C')),
          Stack.of(Crate('P')).push(Crate('D')).push(Crate('N')).push(Crate('Z'))
        )
      )
    )
  }

  test("Stacks.from (small input D)") {
    val filledCrateStacks = List(
      "[~] [~] [Z]",
      "[~] [~] [N]",
      "[M] [~] [D]",
      "[C] [~] [P]",
      " 1   2   3 "
    )
    assertEquals(
      Stacks.from(filledCrateStacks),
      Some(
        Stacks(
          Stack.of(Crate('C')).push(Crate('M')),
          Stack.empty[Crate],
          Stack.of(Crate('P')).push(Crate('D')).push(Crate('N')).push(Crate('Z'))
        )
      )
    )
  }

  test("Stacks.from (small input E)") {
    val filledCrateStacks = List(
      "[~] [~] [Z]",
      "[~] [~] [N]",
      "[~] [~] [D]",
      "[C] [M] [P]",
      " 1   2   3 "
    )
    assertEquals(
      Stacks.from(filledCrateStacks),
      Some(
        Stacks(
          Stack.of(Crate('C')),
          Stack.of(Crate('M')),
          Stack.of(Crate('P')).push(Crate('D')).push(Crate('N')).push(Crate('Z'))
        )
      )
    )
  }

  test("Move.from(\"move 4 from 1 to 9\") returns a valid Move") {
    assertEquals(
      Move.from("move 4 from 1 to 9"),
      Some(Move(n = 4, from = 1, to = 9))
    )
  }

  test("Move.from(\"move 0 from 1 to 9\") returns None") {
    assertEquals(Move.from("move 0 from 1 to 9"), None)
  }

  test("Move.from(\"move 4 from 0 to 9\") returns None") {
    assertEquals(Move.from("move 4 from 0 to 9"), None)
  }

  test("Move.from(\"move 4 from 10 to 9\") returns None") {
    assertEquals(Move.from("move 4 from 10 to 9"), None)
  }

  test("Move.from(\"move 4 from 1 to 0\") returns None") {
    assertEquals(Move.from("move 4 from 1 to 0"), None)
  }

  test("Move.from(\"move 4 from 1 to 10\") returns None") {
    assertEquals(Move.from("move 4 from 1 to 10"), None)
  }

  test("performing BaseMove(from = 2, to = 1) on Stacks with non-empty 'from' Stack produces valid Stacks") {
    val stacks = Stacks(
      Stack.of(Crate('Z')).push(Crate('N')),
      Stack.of(Crate('M')).push(Crate('C')).push(Crate('D')),
      Stack.of(Crate('P'))
    )
    assertEquals(
      BaseMove(from = 2, to = 1).performOn(stacks),
      Some(
        Stacks(
          Stack.of(Crate('Z')).push(Crate('N')).push(Crate('D')),
          Stack.of(Crate('M')).push(Crate('C')),
          Stack.of(Crate('P'))
        )
      )
    )
  }

  test("performing BaseMove(from = 2, to = 1) on Stacks with empty 'from' Stack produces valid Stacks") {
    val stacks = Stacks(
      Stack.of(Crate('C')).push(Crate('M')),
      Stack.empty[Crate],
      Stack.of(Crate('P')).push(Crate('D')).push(Crate('N')).push(Crate('Z'))
    )
    assertEquals(BaseMove(from = 2, to = 1).performOn(stacks), None)
  }

  test("BaseMovePlan.from MovePlan produces valid plan") {
    val movePlan = MovePlan(
      Move(n = 1, from = 2, to = 1),
      Move(n = 3, from = 1, to = 3),
      Move(n = 2, from = 2, to = 1),
      Move(n = 1, from = 1, to = 2)
    )
    assertEquals(
      BaseMovePlan.from(movePlan),
      BaseMovePlan(
        BaseMove(from = 2, to = 1),
        BaseMove(from = 1, to = 3),
        BaseMove(from = 1, to = 3),
        BaseMove(from = 1, to = 3),
        BaseMove(from = 2, to = 1),
        BaseMove(from = 2, to = 1),
        BaseMove(from = 1, to = 2)
      )
    )
  }

  test("performing BaseMovePlan on Stacks produces valid Stacks") {
    val baseMovePlan = BaseMovePlan.from(
      MovePlan(
        Move(n = 1, from = 2, to = 1),
        Move(n = 3, from = 1, to = 3),
        Move(n = 2, from = 2, to = 1),
        Move(n = 1, from = 1, to = 2)
      )
    )
    val stacks = Stacks(
      Stack.of(Crate('Z')).push(Crate('N')),
      Stack.of(Crate('M')).push(Crate('C')).push(Crate('D')),
      Stack.of(Crate('P'))
    )
    assertEquals(
      baseMovePlan.performOn(stacks),
      Stacks(
        Stack.of(Crate('C')),
        Stack.of(Crate('M')),
        Stack.of(Crate('P')).push(Crate('D')).push(Crate('N')).push(Crate('Z'))
      )
    )
  }

  test("getStacksTopCratesAfterPerformingBaseMovePlan(bigInput) returns valid value") {
    assertEquals(getStacksTopCratesAfterPerformingBaseMovePlan(bigInput), Some("TQRFCBSJJ"))
  }

  test("performing MovePlan on Stacks produces valid Stacks") {
    val movePlan = MovePlan(
      Move(n = 1, from = 2, to = 1),
      Move(n = 3, from = 1, to = 3),
      Move(n = 2, from = 2, to = 1),
      Move(n = 1, from = 1, to = 2)
    )
    val stacks = Stacks(
      Stack.of(Crate('Z')).push(Crate('N')),
      Stack.of(Crate('M')).push(Crate('C')).push(Crate('D')),
      Stack.of(Crate('P'))
    )
    assertEquals(
      movePlan.performOn(stacks),
      Stacks(
        Stack.of(Crate('M')),
        Stack.of(Crate('C')),
        Stack.of(Crate('P')).push(Crate('Z')).push(Crate('N')).push(Crate('D'))
      )
    )
  }

  test("getStacksTopCratesAfterPerformingMovePlan(bigInput) returns valid value") {
    assertEquals(getStacksTopCratesAfterPerformingMovePlan(bigInput), Some("RMHFJNVFP"))
  }

}
object Day5Suite {

  val bigInput: List[String] = FileLoader.getLinesFromFile("src/test/scala/adventofcode22/day5_input.txt")

}
