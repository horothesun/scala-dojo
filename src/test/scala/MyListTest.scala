import MyList._
import org.scalatest.funsuite.AnyFunSuite

final class MyListTest extends AnyFunSuite {

  test("tail is None for MyNil") {
    assert(tail(MyNil).isEmpty)
  }

}
