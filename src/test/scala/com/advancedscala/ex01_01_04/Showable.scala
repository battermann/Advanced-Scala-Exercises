package com.advancedscala.ex01_01_04

trait Showable[A] {
  def format(v:A) : String
}

object ShowDefaults {
  implicit val printString = new Showable[String] {
    def format(v: String) = v
  }

  implicit val printInt = new Showable[Int] {
    def format(v:Int) = v.toString
  }
}

object Show {

  def format[A](input: A)(implicit showable: Showable[A]): String = {
    showable.format(input)
  }

  def show[A](input: A)(implicit shower: Showable[A]): Unit = {
    println(shower.format(input))
  }
}

object ShowSyntax {
  implicit class ShowOps[A](value: A) {
    def format(implicit showable: Showable[A]) : String = {
      Show.format(value)
    }

    def show(implicit showable: Showable[A]): Unit = {
      Show.show(value)
    }
  }
}
