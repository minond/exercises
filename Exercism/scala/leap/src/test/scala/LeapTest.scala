import org.scalatest._

class LeapTest extends FunSuite {
  test ("vanilla leap year") {
    assert(Year.isLeap(1996))
  }

  test ("any old year") {
    assert(!Year.isLeap(1997))
  }

  test("an even year") {
    assert(!Year.isLeap(1986))
  }

  test ("century") {
    assert(!Year.isLeap(1900))
  }

  test ("exceptional century") {
    assert(Year.isLeap(2000))
  }

  test("exceptional century that is no millenium") {
    assert(Year.isLeap(1600))
  }
}
