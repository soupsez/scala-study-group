package fp.chapter2

import org.scalatest._
import fp.chapter2.Chapter2._


class TestChapter2 extends FunSuite{

  test("factorial") {
      assert(factorial(4) == 24)
      assert(factorial(5) == 120)
      assert(factorial(90) == 1.4857159644817605E138)
    }

  test("fibonacci") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
  }

  test("formatresults") {
    assert(formatResults(fib(2)) == "The result is 1")
    assert(formatResults(factorial(5)) == "The result is 120.0")
  }

  test("findFirst") {
    val l = List("a","b","c","d")
    val n = List(1,2,3,4,5)
    assert(findFirst(l,"b") == true)
    assert(findFirst(l,"e") == false)
    assert(findFirst(n,3) == true)
  }

  test("isSorted") {
    assert(isSorted(Array(1,2,3,4),(a:Int,b:Int) => (a < b)) == true)
  }

  test("partial") {
    val add = (a:Int,b:Int) => a + b
    val part1 = partial1(3,add)
    assert(part1(2) == 5)
  }

  test("curry") {
    val add = (a:Int,b:Int) => (a+b)
    val c = curry(add)
    assert(c(1)(2) == 3)
  }

  test("uncurry") {
    val cAdd = (a:Int) => ((b:Int) => a + b)
    val uc = uncurry(cAdd)
    assert(uc(1,2) == 3)
  }

  test("compose") {
    val inc = (a:Int) => a + 1
    val dec = (b:Int) => b - 1
    val incThenDec = compose(dec, inc)
    val decThenInc = compose(inc, dec)
    assert(incThenDec(23) == 23)
    assert(decThenInc(23) == 23)
  }
}
