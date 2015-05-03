package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("Simple expression of two values") {
    val a = Literal(8)
    val b = Literal(2)
    val plus = Calculator.computeValues(Map("a" -> Signal { Plus(a, b) }))
    val minus = Calculator.computeValues(Map("a" -> Signal { Minus(a, b) }))
    val times = Calculator.computeValues(Map("a" -> Signal { Times(a, b) }))
    val divide = Calculator.computeValues(Map("a" -> Signal { Divide(a, b) }))

    assert(plus("a")() == 10)
    assert(minus("a")() == 6)
    assert(times("a")() == 16)
    assert(divide("a")() == 4)
  }

  test("Ref expression with simple values") {
    val a = Literal(8)
    val b = Literal(2)
    val c = Ref("a")

    val result = Calculator.computeValues(Map(
      "a" -> Signal { Plus(a, b) }, 
      "b" -> Signal { Times(c, b) }
    ))

    assert(result("a")() == 10)
    assert(result("b")() == 20)
  }

  test("Ref expressions with more complicated values") {
    val a = Plus(Literal(1), Literal(2)) // 3
    val b = Times(Ref("a"), Literal(3))  // 9
    val c = Times(Ref("a"), Ref("b")) // 27
    val d = Divide(Minus(Ref("c"), Literal(3)), Plus(Literal(3), Literal(0))) // 8

    val result = Calculator.computeValues(Map(
      "a" -> Signal { a }, 
      "b" -> Signal { b },
      "c" -> Signal { c },
      "d" -> Signal { d }
    ))

    assert(result("a")() == 3)
    assert(result("b")() == 9)
    assert(result("c")() == 27)
    assert(result("d")() == 8)
  }

  test("Ref variables that don't exists returns NaN") {
    val result = Calculator.computeValues(Map(
      "a" -> Signal { Plus(Ref("b"), Literal(2)) }
    ))

    assert(result("a")().isNaN)
  }

  test("Ref expression with cyclic dependencies throws exception") {
    val result = Calculator.computeValues(Map(
      "a" -> Signal { Plus(Ref("b"), Literal(2)) }, 
      "b" -> Signal { Plus(Ref("a"), Literal(3)) }
    ))

    assert(result("a")().isNaN)
  }


}
