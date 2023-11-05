package adventofcode22

import cats.implicits._
import munit.ScalaCheckSuite

class StackSuite extends ScalaCheckSuite {

  test("Stack.of(1).push(2).push(3).toList returns List(3, 2, 1)") {
    assertEquals(
      Stack.of(1).push(2).push(3).toList,
      List(3, 2, 1)
    )
  }

}
