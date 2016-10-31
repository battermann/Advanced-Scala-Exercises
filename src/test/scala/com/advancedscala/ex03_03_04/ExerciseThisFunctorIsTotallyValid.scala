package com.advancedscala.ex03_03_04

object result {

  sealed trait Result[+A]

  final case class Success[A](value: A) extends Result[A]

  final case class Warning[A](value: A, message: String) extends Result[A]

  final case class Failure(message: String) extends Result[Nothing]

  object Result {
    import cats.Functor

    implicit val resultFunctor = new Functor[Result] {
      def map[A, B](input: Result[A])(f: A => B): Result[B] = input match {
        case Success(v) => Success(f(v))
        case Warning(v, msg) => Warning(f(v), msg)
        case Failure(msg) => Failure(msg)
      }
    }

    implicit class ResultOps[A](v: A) {
      def success : Result[A] = Success(v)
      def warning(msg: String) : Result[A]= Warning(v, msg)
    }

    implicit class FailureOps[A](msg: String) {
      def failure : Result[A] = Failure(msg)
    }
  }
}

import cats.syntax.all._
import com.advancedscala.ex04_03_05.result.Result.ResultOps
import com.advancedscala.ex04_03_05.result.{Success, Warning}
import org.scalatest.FunSuite

class ExerciseThisFunctorIsTotallyValid extends FunSuite {
    test("result") {
      val succ = 42.success
      assert(succ.map(_ + 1) == Success(43))

      assert(42.warning("caution!").map(_ + 1) == Warning(43, "caution!"))
    }
}
