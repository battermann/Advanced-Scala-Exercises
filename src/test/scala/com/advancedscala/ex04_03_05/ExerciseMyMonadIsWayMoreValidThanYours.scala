package com.advancedscala.ex04_03_05

import org.scalatest.FunSuite

object result {

  sealed trait Result[+A]
  final case class Success[A](value: A) extends Result[A]
  final case class Warning[A](value: A, message: String) extends Result[A]
  final case class Failure(message: String) extends Result[Nothing]

  object Result {
    import cats.Monad

    implicit val resultMonad = new Monad[Result] {
      def pure[A](value: A): Result[A] =
        Success(value)

      def flatMap[A, B](result: Result[A])(func: A => Result[B]): Result[B] =
        result match {
          case Success(value) =>
            func(value)
          case Warning(value, message1) =>
            func(value) match {
              case Success(value) =>
                Warning(value, message1)
              case Warning(value, message2) =>
                Warning(value, s"$message1 $message2")
              case Failure(message2) =>
                Failure(s"$message1 $message2")
            }
          case Failure(message) =>
            Failure(message)
        }

      def tailRecM[A, B](a: A)(f: A => Result[Either[A, B]]): Result[B] = defaultTailRecM(a)(f)
    }

    implicit class ResultOps[A](v: A) {
      def success: Result[A] = Success(v)
      def warning(msg: String): Result[A]= Warning(v, msg)
    }

    implicit class FailureOps[A](msg: String) {
      def failure : Result[A] = Failure(msg)
    }
  }
}

import result.Result.ResultOps
import cats.syntax.all._

class ExerciseMyMonadIsWayMoreValidThanYours extends FunSuite {
  test("monad") {
    val succ = 1.success

    val res = succ.flatMap(x => (x + 5).warning("ACHTUNG!"))

    assert(res == 6.warning("ACHTUNG!"))
  }
}
