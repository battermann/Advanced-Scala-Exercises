package com.advancedscala.ex01_03_05

import cats.Eq
import org.scalatest.FunSuite

class ExerciseEquality extends FunSuite {

  object cat {
    final case class Cat(name: String, age: Int, color: String)
    object Cat {
      implicit val catEqual = Eq.instance[Cat] { (cat1, cat2) =>
        List(
          cat1.name  === cat2.name,
          cat1.age   === cat2.age,
          cat1.color === cat2.color
        ).forall(_ == true)
      }
    }
  }
  import cat._

  val cat1 = Cat("Garfield", 35, "orange and black")
  val cat2 = Cat("Heathcliff", 30, "orange and black")

  val optionCat1: Option[Cat] = Some(cat1)
  val optionCat2: Option[Cat] = None

  test("eq exercise") {
    import cats.syntax.option._

    assert(cat1 === cat1)
    assert(cat1 !== cat2)
    assert(cat1.some === optionCat1)
    assert(cat1 !== optionCat2)
    assert(cat2 !== optionCat1)
    assert(optionCat1 === optionCat1)
    assert(cat2 !== optionCat2)
    assert(optionCat1 !== optionCat2)
  }
}
