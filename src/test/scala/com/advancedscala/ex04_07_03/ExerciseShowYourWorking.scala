package com.advancedscala.ex04_07_03

import cats.data.Writer
import cats.syntax.all._
import cats.instances.all._
import org.scalatest.FunSuite

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Factorial {
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))

    println(s"fact $n $ans")

    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialWithWriter(n: Int): Int = {

    def factorialWithWriterRec(n: Int): Logged[Int] = {
      if(n == 0)
        1.writer(Vector(s"fact 0 1"))
      else
        factorialWithWriterRec(n - 1)
          .flatMap(v => (v * n)
            .writer(Vector(s"fact $n ${v*n}")))
    }

    val (log, ans) = factorialWithWriterRec(n).run
    log.foreach(println)
    ans
  }
}

class ExerciseShowYourWorking extends FunSuite {
  test("factorial") {
    import Factorial._

    Await.result(Future.sequence(Vector(
      Future(factorialWithWriter(5)),
      Future(factorialWithWriter(5))
    )), Duration.Inf)
  }
}
