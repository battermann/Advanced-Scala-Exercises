package com.advancedscala.ex02_05_05

import cats.Monoid
import cats.syntax.all._
import cats.instances.all._
import org.scalatest.FunSuite

class ExeriseAddingAllTheThings extends FunSuite {

  object order {

    case class Order(totalCost: Double, quantity: Double)

    object Order {
      implicit val orderMonoid = new Monoid[Order] {
        def empty() = Order(0, 0)

        def combine(a: Order, b: Order) = Order(a.totalCost + b.totalCost, a.quantity + b.quantity)
      }
    }
  }
  import order._

  def add[A: Monoid](items: List[A]): A = items.combineAll

  test("add") {
    assert(add(List(1,2,3,4,5,6,7,8,9,10)) == 55)
    assert(add(List(1.some, None, 3.some, None, 5.some)) == 9.some)
    assert(add(List(Order(1,10), Order(2,20), Order(3,30))) == Order(6, 60))
  }
}
