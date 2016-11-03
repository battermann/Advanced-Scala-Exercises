package com.advancedscala.casestudy08

import cats.Monoid
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.FunSuite

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class PygmyHadoop extends FunSuite {
  def foldMap[A, B: Monoid](values: Iterable[A])(func: A => B = (a: A) => a): B =
    values.foldLeft(Monoid[B].empty)(_ |+| func(_))

  def foldMapP[A, B: Monoid]
  (values: Iterable[A])
  (func: A => B = (a: A) => a)
  (implicit ec: ExecutionContext): Future[B] = {
    val size = (values.size.toDouble / Runtime.getRuntime.availableProcessors).ceil.toInt

    val foldedPartials = values
      .grouped(size)
      .map(partial => Future { partial })
      .map(f => f.map(partial => foldMap(partial)(func)))

    Future.sequence(foldedPartials).map(x => foldMap(x.toIterable)(identity))
  }

  test("foldMap") {
    assert(foldMap(List(1, 2, 3))(identity) == 6)
    assert(Await.result(foldMapP(1 to 1000)(identity), Duration.Inf) ==  500500)
  }
}
