package com.advancedscala.ex06_04_04

import org.scalatest.FunSuite
import cats.data.{Validated, Xor}
import cats.instances.all._
import cats.syntax.all._

import scala.util.Try



object Validation {

  type Error = Vector[String]

  type Result[A] = Validated[Error, A]

}

case class User(name: String, age: Int)

import Validation.Result

class FormValidation extends FunSuite {
  def readName(request: Map[String, String]): Result[String] = {

    def getName(request: Map[String, String]): Xor[String, String] =
      Xor.fromOption(request.get("name"), "The name must be specified.")

    def checkNotEmpty(name: String): Xor[String, String] =
      name.right.ensure("The name must not be blank.")(x => x != null && !x.isEmpty)

    getName(request)
      .flatMap(checkNotEmpty)
      .leftMap(err => Vector(err))
      .toValidated
  }

  def readAge(request: Map[String, String]): Result[Int] = {

    def getAge(request: Map[String, String]): Xor[String, String] =
      Xor.fromOption(request.get("age"), "The age must be specified.")

    def tryParse(age: String): Xor[String, Int] =
      Xor.fromOption(Try(age.toInt).toOption, "The the age must be a valid integer.")

    def checkNonNegative(age: Int): Xor[String, Int] =
      age.right.ensure("The the age must be non-negative integer.")(_ >= 0)

    getAge(request)
      .flatMap(tryParse)
      .flatMap(checkNonNegative)
      .leftMap(err => Vector(err))
      .toValidated
  }

  test("readName") {
    assert(readName(Map("name" -> "peter")) == "peter".valid[Vector[String]])
    assert(readName(Map("name" -> "")) == Vector("The name must not be blank.").invalid[String])
    assert(readName(Map("age" -> "123")) == Vector("The name must be specified.").invalid[String])

    assert(readAge(Map("age" -> "30")) == 30.valid[Vector[String]])
    assert(readAge(Map("age" -> "0")) == 0.valid[Vector[String]])
    assert(readAge(Map("age" -> "-1")) == Vector("The the age must be non-negative integer.").invalid[Int])
    assert(readAge(Map("age" -> "xyz")) == Vector("The the age must be a valid integer.").invalid[Int])
    assert(readAge(Map("name" -> "peter")) == Vector("The age must be specified.").invalid[Int])
  }
}
