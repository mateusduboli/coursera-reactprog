package calculator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, ShouldMatchers}

/**
 * Created by mateus on 6/14/15.
 */
@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite with ShouldMatchers {

  test("should calculate an accurate delta for (1,1,1)") {
    val delta = Polynomial.computeDelta(Var(1), Var(1), Var(1))
    assert(delta() == -3.0)
  }

  test("should calculate an accurate delta for (1,2,1)") {
    val delta = Polynomial.computeDelta(Var(1), Var(2), Var(1))
    assert(delta() == 0.0)
  }

  test("should calculate an accurate delta for (1,3,1)") {
    val delta = Polynomial.computeDelta(Var(2), Var(3), Var(1))
    assert(delta() == 1.0)
  }

  test("should return a empty set when delta is negative") {
    val roots = Polynomial.computeSolutions(Var(1), Var(1), Var(1), Var(-3.0))
    assert(roots() == Set.empty)
  }

  test("should return a single element set when delta is zero") {
    val roots = Polynomial.computeSolutions(Var(1), Var(2), Var(1), Var(.0))
    assert(roots() == Set(-1.0))
  }

  test("should return a two element set when delta is greater then zero") {
    val roots = Polynomial.computeSolutions(Var(2), Var(3), Var(1), Var(5.0))
    assert(roots().size == 2)
  }
}
