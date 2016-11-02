package com.advancedscala.ex05_03

import cats.data.{Xor, XorT}
import org.scalatest.FunSuite
import cats.instances.all._
import cats.syntax.all._
import scala.concurrent.duration._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class ExerciseUsingMonadTransformers extends FunSuite {

  type FutureXor[A] = XorT[Future, String, A]

  val loadAverages = Map(
    "a.example.com" -> 0.1,
    "b.example.com" -> 0.5,
    "c.example.com" -> 0.2
  )

  def getLoad(hostname: String): FutureXor[Double] = {
    loadAverages.get(hostname) match {
      case Some(v) => v.pure[FutureXor]
      case _ => XorT[Future, String, Double](Future.successful(Xor.left(s"host $hostname unreachable")))
    }
  }

  def getMeanLoad(hostnames: List[String]): FutureXor[Double] = {
    if(hostnames.isEmpty)
      XorT[Future, String, Double](Future.successful(Xor.left("no hosts specified")))
    else
      hostnames.map(getLoad).sequence.map(x => x.sum / hostnames.length)
  }

  def report[A](input: FutureXor[A]): String = {
    Await.result(input.value, 2.seconds).fold(
      err => s"[FAILURE] $err",
      res => s"[SUCCESS] $res"
    )
  }

  test("future xor") {
    println(report(getMeanLoad(List("a.example.com", "b.example.com"))))
    println(report(getMeanLoad(List("a.example.com", "c.example.com"))))
    println(report(getMeanLoad(List("a.example.com", "d.example.com"))))
  }
}
