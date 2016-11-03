package com.advancedscala.ex07_01_02

import cats.Monoid
import cats.implicits._

import org.scalatest.FunSuite

class ScaffoldingOtherMethods extends FunSuite {

  def map[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(List[B]())((el, acc) => f(el) :: acc)

  def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(List[B]())((el, acc) => f(el) ++ acc)

  def filter[A](list: List[A])(p: A => Boolean) =
    list.foldRight(List[A]())((el, acc) => if (p(el)) el :: acc else acc)

  def sum[A: Monoid](list: List[A]): A = list.combineAll

  test("map") {
    assert(List(1,2,3).map(_ + 1) == map(List(1,2,3))(_ + 1))
  }

  test("flatMap") {
    def f = (n: Int) => (0 until n).map(_ => n).toList
    assert(List(1,2,3).flatMap(f) == flatMap(List(1,2,3))(f))
  }

  test("filter") {
    def p = (n: Int) => n % 2 == 0
    assert(List(1,2,3,4).filter(p) == filter(List(1,2,3,4))(p))
  }

  test("sum") {
    assert(List(1.0, 2.0, 3.5).sum == sum(List(1.0, 2.0, 3.5)))
  }
}
