package com.advancedscala.ex01_02_05

import com.advancedscala.ex01_01_04._
import com.advancedscala.ex01_01_04.ShowSyntax._
import com.advancedscala.ex01_01_04.ShowDefaults._

import org.scalatest.FunSuite

class ExerciseCatShow extends FunSuite {

  object cat {

    final case class Cat(name: String, age: Int, color: String)

    object Cat {
      implicit val catWithShow = new Showable[Cat] {
        def format(v: Cat) = {
          val name = Show.format(v.name)
          val age = Show.format(v.age)
          val color = Show.format(v.color)
          s"$name is a $age year old $color cat"
        }
      }
    }
  }

  import cat._

  test("showable") {
    val c = Cat("Minki", 5, "black")
    c.show
    assert("Minki is a 5 year old black cat" === c.format)
  }
}
