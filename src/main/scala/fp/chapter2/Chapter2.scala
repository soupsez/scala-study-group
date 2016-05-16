package fp.chapter2

import scala.annotation.tailrec

/**
  * Created by sup on 4/30/16.
  */
object Chapter2 {
  def factorial(i: Double): Double = {
    @annotation.tailrec
    def go(acc: Double, current: Double): Double = {
      current match {
        case 1 => acc
        case _ => go(acc * current, current - 1)
      }
    }
    go(1, i)
  }


  def fib(i: Int): Int = {
    @tailrec
    def go(current: Int, next: Int, counter: Int): Int = {
      counter match {
        case 0 => current
        case 1 => next
        case _ => go(current, current + next, counter - 1)
      }
    }
    go(0, 1, i)
  }

  def findFirst[T](l: List[T], s: T):Boolean = {
    l match {
      case Nil => false
      case x::y if(x == s) => true
      case x::y => findFirst(y,s)
    }
  }

  def formatResults[T](func: => T):String = {
    val result = func
    "The result is %s".format(result)
  }

  def isSorted[A](arr:Array[A], f:(A,A) => Boolean):Boolean = {
    val len = arr.length
    def go(idx:Int):Boolean = idx match {
      case _ if(idx+1 == len) => true
      case _ if(f(arr(idx),arr(idx+1)) == false) => false
      case _ => go(idx+1)
    }
    go(0)
  }

  def partial1[A,B,C](a:A, f: (A,B) => C): B => C = {
    (b:B) => f(a,b)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def uncurry[A,B,C](f: A => (B => C)): (A,B) => C = {
    (a,b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g:A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]):Unit = {
    formatResults(fib(5))
    formatResults(factorial(4))
  }
}
