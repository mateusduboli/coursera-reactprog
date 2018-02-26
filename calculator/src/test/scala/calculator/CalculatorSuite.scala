package calculator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, ShouldMatchers}
import calculator.Calculator._

import scala.Double

/**
 * Created by mateus on 6/15/15.
 */
@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSpec with ShouldMatchers {

  describe("Calculator") {
    describe("#computeValues") {
      it("should evaluate literals") {
        val inputExprs: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(0.0)))
        val output = Calculator.computeValues(inputExprs)
        output("a")() shouldBe 0.0 +- 0.00001
      }

      it("should retrive a referenced values") {
        val inputExprs: Map[String, Signal[Expr]] = Map(
          "a" -> Var(Literal(0.0)),
          "b" -> Var(Ref("a"))
        )
        val output = Calculator.computeValues(inputExprs)
        output("a")() shouldBe 0.0 +- 0.00001
        output("b")() shouldBe 0.0 +- 0.00001
      }

      it("should calculate operations") {
        val inputExprs: Map[String, Signal[Expr]] = Map(
          "a" -> Var(Plus(Literal(1.0), Literal(1.0))),
          "b" -> Var(Minus(Literal(1.0), Literal(1.0))),
          "c" -> Var(Times(Literal(1.0), Literal(1.0))),
          "d" -> Var(Divide(Literal(1.0), Literal(1.0)))
        )
        val output = Calculator.computeValues(inputExprs)
        output("a")() shouldBe 2.0 +- 0.00001
        output("b")() shouldBe 0.0 +- 0.00001
        output("c")() shouldBe 1.0 +- 0.00001
        output("d")() shouldBe 1.0 +- 0.00001
      }

      it("should identify simple cyclic references") {
        val aVal: Var[Expr] = Var(Literal(0.0))
        val inputExprs: Map[String, Signal[Expr]] = Map(
          "a" -> aVal
        )
        val results = Calculator.computeValues(inputExprs)
        aVal() = Ref("a")
        for ((_, exp) <- results) (exp().isNaN shouldBe true)
      }

      it("should identify transitive cyclic references") {
        val values: Seq[Var[Expr]] = List.fill(10)(Var(Literal(0.0)))
        val keys: Seq[String] = (0 until 10).map { n: Int => ('a' + n).toChar.toString() }
        val refs = keys.zip(values).toMap
        val results = Calculator.computeValues(refs)
        refs("a")() = Ref("b")
        refs("b")() = Ref("c")
        refs("c")() = Ref("d")
        refs("d")() = Ref("e")
        refs("e")() = Ref("f")
        refs("f")() = Ref("g")
        refs("g")() = Ref("h")
        refs("h")() = Ref("i")
        refs("i")() = Ref("j")
        refs("j")() = Ref("a")
        for ((_, exp) <- results) (exp().isNaN shouldBe true)
      }
    }
  }
}
