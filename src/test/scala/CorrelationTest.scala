import org.scalatest.FunSuite

import Correlation._

class CorrelationTest extends FunSuite {

  test("correlation") {
    val xs = List(15, 12, 8, 8, 7, 7, 7, 6, 5, 3) map (_.toDouble)
    val ys = List(10, 25, 17, 11, 13, 17, 20, 13, 9, 15) map (_.toDouble)

    assertResult(0.145) {
      roundAt(3)(correlation(xs, ys))
    }
  }

  test("sd") {
    assertResult(2.983) {
      val xs = List(9, 2, 5, 4, 12, 7, 8, 11, 9, 3, 7, 4, 12, 5, 4, 10, 9, 6, 9, 4) map (_.toDouble)

      roundAt(3)(sd(xs))
    }
  }

  test("expectation") {
    assertResult(3.5) {
      val xs = 1 to 6 map (_.toDouble)

      roundAt(1)(expectation(xs))
    }
  }

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p)

    math.round(n * s) / s
  }
}
