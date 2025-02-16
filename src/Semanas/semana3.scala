package Semanas

//import scala.::
import scala.annotation.tailrec
//import scala.collection.immutable.::

object semana3 extends App {

  val xs = List(1, 2, 3, 4)
  println((xs foldLeft "za")(_ + _))
  println((xs foldRight "za")(_ + _))


  def factorial(n: Int): Int = {
    @tailrec
    def fact(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else fact(n - 1, n * acc)
    }

    fact(n, 1)
  }

  def app(f: Int => Int, t: (Int, Int)): (Int, Int) = {
    (f(t._1), f(t._2))
  }

  //sum de k until n of f(k)
  def sum(f: Int => Int, k: Int, n: Int): Int = {
    if (k > n) 0
    else f(k) + sum(f, k + 1, n)
  }

  def posElems1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x :: xs1 => if (x > 0) x :: posElems1(xs1)
    else posElems1(xs1)
  }

  def scaleList(xs: List[Int], factor: Int) =
    xs map (x => x * factor)

  def posElems(xs: List[Int]) =
    xs filter (x => x > 0)

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case List() => ys
    case x :: xs1 => x :: append(xs1, ys)
  }

  def isOdd(n: Int) = n % 2 != 0

  val (x, y) = xs partition isOdd

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _) //foldRight is the same as foldLeft but it starts from the right

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((x, y) => f(x) :: y)

}