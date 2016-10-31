package com.advancedscala.ex02_03

import org.scalatest.FunSuite

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid

  def combine[A](a1: A, a2: A)(implicit x: Semigroup[A]) = {
    x.combine(a1, a2)
  }

  def empty[A]()(implicit x: Monoid[A]) = {
    x.empty
  }
}

object SemigroupDefaults {
  implicit val boolAndDefault = new Semigroup[Boolean] {
    def combine(b1: Boolean, b2: Boolean) = b1 && b2
  }
}

object MonoidDefaults {
  implicit val boolAndMonoid = new Monoid[Boolean] {
    def combine(b1: Boolean, b2: Boolean) = SemigroupDefaults.boolAndDefault.combine(b1, b2)
    def empty() = true
  }
}

object Semigroup {
  def combine[A](a1: A, a2: A)(implicit x: Semigroup[A]) = {
    x.combine(a1, a2)
  }
}

object MonoidSyntax {
  implicit class MonoidOps[A](value: A) {
    def combine(other: A)(implicit semigroup: Semigroup[A]) : A = semigroup.combine(value, other)
    def empty()(implicit monoid: Monoid[A]) : A = monoid.empty
  }
}

class ExerciseTheTruthAboutMonoids extends FunSuite {

  import MonoidDefaults._
  import MonoidSyntax._

  test("monoids &&") {
    assert(true.combine(false) === false)
    assert(true.combine(true) === true)
    assert(true.empty() === true)
  }
}

class ExerciseTheTruthAboutMonoids2 extends FunSuite {

  import MonoidSyntax._

  test("monoids ||") {

    implicit val boolOrMonoid = new Monoid[Boolean] {
      def combine(b1: Boolean, b2: Boolean) = b1 || b2
      def empty = false
    }

    assert(true combine false === true)
    assert(true.empty() === false)
    assert(false.combine(false) === false)
  }
}
