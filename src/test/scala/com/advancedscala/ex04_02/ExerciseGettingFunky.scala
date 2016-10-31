package com.advancedscala.ex04_02

import org.scalatest.FunSuite

import scala.language.higherKinds




class ExerciseGettingFunky extends FunSuite {
  def flatMap[F[_], A, B](value: F[A])(func: A => F[B]): F[B] = ???

  def pure[F[_], A](value: A): F[A] = ???

  def map[F[_], A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(x => pure(func(x)))

  test("map in terms of flatMap and pure") {

  }
}
