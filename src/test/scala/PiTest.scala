import org.scalatest.FunSuite

/**
  * Created by mlamprea on 27/11/16.
  */
class PiTest extends FunSuite {
  test("Getting value of Pi") {
    Pi.calculate(4, 10000, 10000)
  }
}
