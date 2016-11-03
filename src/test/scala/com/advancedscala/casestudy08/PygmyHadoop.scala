package com.advancedscala.casestudy08

import cats.data.Xor
import cats.{Id, Monad, Monoid}
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.FunSuite

import scala.language.higherKinds
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

  def foldMapM[A, M[_]: Monad, B: Monoid](iter: Iterable[A])(f: A => M[B] = (a: A) => a.pure[Id]): M[B] =
    iter.foldLeft(Monad[M].pure(Monoid[B].empty))((acc, el) => for {
      x1 <- acc
      x2 <- f(el)
    } yield x1 |+| x2)

  def foldMap2[A, B: Monoid](values: Iterable[A])(func: A => B = (a: A) => a): B =
    foldMapM(values)(x => func(x).pure[Id])

  type ErrorOr[A] = String Xor A

  test("foldMapM Xor") {
    val seq = Seq("1", "2", "3")

    assert(foldMapM[String, ErrorOr, Int](seq)(x => Xor.catchOnly[NumberFormatException](x.toInt).leftMap(_ => s"Cannot parse input string: $x.")) == 6.right[String])

    val seqInValid = Seq("1", "abc", "3")

    assert(foldMapM[String, ErrorOr, Int](seqInValid)(x => Xor.catchOnly[NumberFormatException](x.toInt).leftMap(_ => s"Cannot parse input string: $x.")) == "Cannot parse input string: abc.".left[Int])
  }

  test("foldMap") {
    assert(foldMap(List(1, 2, 3))() == 6)
  }

  test("foldMapP") {
    assert(Await.result(foldMapP(1 to 1000)(identity), Duration.Inf) ==  500500)
  }

  test("foldMapM") {
    val seq = List(1, 2, 3)
    assert(foldMapM(seq)(a => Option(a)) == Some(6))

    assert(foldMapM(seq)() == 6)
  }

  test("foldMap2") {
    assert(foldMap2(List(1, 2, 3))() == 6)
  }
}
