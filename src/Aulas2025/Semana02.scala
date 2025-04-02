package Aulas2025

object Semana02 extends App{

  def factorial (n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }

  def factorial1 (n: Int): Int = {
    if(n == 0) 1
    else n * factorial1(n-1)
  }

  def fibonacci(n:Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(n-1) + fibonacci(n-2)
  }

  def listOfDupes[A](x: A, length: Int): List[A] = {
    if(length < 1 ) Nil
    else x :: listOfDupes(x, length-1)
  }

  println("List of Dupes  " + listOfDupes[Int](3, 4) + "\n" + "next")

  def transf(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case x::xs::xxs => xs::x::transf(xxs)
    }
  }
  val list = List(1, 2, 3, 4, 5, 6)
  println("A) " + transf(list))

  def multiplyEachInList(list: List[Int]): Int ={
    list match{
      case Nil => 0
      case x::Nil => x
      case x::xs => x * multiplyEachInList(xs)
    }
  }
  println("B) " + multiplyEachInList(List(1, 2, 3, 4, 5, 6)))
  print("C) ")
  def addAtEnd[A](list: List[A], n: A): List[A] = {
    list match {
      case Nil => List(n)
      case x :: xs => x :: addAtEnd(xs, n)
    }
  }

  def addAtEnd1[A](list: List[A], n: A): List[A] = {
    list :+ n
  }
  println(addAtEnd(list, 7))
  println("C') " + addAtEnd1(list, 7))
  print("D) ")
  
  def concat[A](list1: List[A], list2: List[A]): List[A] = {
    list1 match {
      case Nil => list2
      case x :: xs => x :: concat(xs, list2)
    }
  }
  println(concat(list, List(7, 8)))
  print("E) ")
  def sumPairs24(list: List[(Int, Int)]): Int = {
    if (list.length < 3) 0
    else if (list.length < 5) {
      list(2)._1 + list(2)._2
    }
    else {
      list(2)._1 + list(2)._2 + sumPairs24(list.drop(3))
    }
  }
  //                         0       1       2       3       4         5
  println(sumPairs24(List((1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12))))
  println(sumPairs24(List((1, 2), (3, 4), (5, 6), (7, 8), (9, 10))))
  println(sumPairs24(List((1, 2), (3, 4), (5, 6), (7, 8))))
  println(sumPairs24(List((1, 2), (3, 4))))
  println(sumPairs24(List((1, 2))))
  println(sumPairs24(List()))
  print("F) ")
  def sumAndLength(list: List[Double]): (Double, Int) = {
    list match{
      case Nil => (0, 0)
      case x::xs => (x + sumAndLength(xs)._1, 1 + sumAndLength(xs)._2)
    }
  }
  println(sumAndLength(List(1, 2, 3, 4, 5, 6)))

  def average(list: List[Double]): Double = {
    val aux = sumAndLength(list)
    aux._1 / aux._2
  }
  println("Average" + average(List(1, 2, 3, 4, 5, 6)))

  println("G) ")
  def divideList(list: List[Double], c: Double): (List[Double], List[Double]) = {
    val list1 = List()
    val list2 = List()
    return divideListAux(list, list1, list2, c)


  }

  def divideListAux(list: List[Double], list1: List[Double], list2: List[Double], c: Double): (List[Double], List[Double]) = {
    list match {
      case Nil => (list1, list2)
      case x::xs => if (x < c) divideListAux(xs, list1:+x, list2, c)
      else divideListAux(xs, list1, list2:+x, c)
    }
  }

  //test
  println("divideList 123456 , 3 " + divideList(List(1, 2, 3, 4, 5, 6), 3))

  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
  val empty = List()

  println("Exercise 2")
  type Entry = (String, String, String)
  type LTelef = List[Entry]
  def emails ( list: LTelef): List[String] = {
    list match {
      case Nil => Nil
      case (_, _, email) :: xs => email :: emails(xs)
    }
  }
  def cell2 (list: LTelef): List[String] = {
    list match {
      case Nil => Nil
      case (_, cell, _) :: xs => if (cell.head == "2") cell :: cell2(xs)
      else cell2(xs)
    }
  }

  //testar cell2

  // Test data
  val phoneBook: LTelef = List(
    ("John Doe", "123456789", "john@example.com"),
    ("Jane Smith", "234567890", "jane@example.com"),
    ("Alice Johnson", "345678901", "alice@example.com"),
    ("Bob Brown", "223344556", "bob@example.com")
  )

  // Test the emails function
  println("Emails: " + emails(phoneBook))

  // Test the cell2 function
  println("Cell numbers starting with '2': " + cell2(phoneBook))

  def search(list: LTelef, name: String): List[(String, String)] = {
    list match {
      case Nil => Nil
      case (n, phone, email) :: xs => if (n == name) (phone, email) :: search(xs, name)
      else search(xs, name)
    }
  }

  // Test the search function
  val searchResult = search(phoneBook, "John Doe")
  println(s"Search result " + searchResult)

  println("Exercício Extra - Polymorphic method " +
    "without List methods take or takeRight")

  def divide[T](list:List[T]): (List[T], List[T]) = {
    val (list1, list2) = list.splitAt(list.length / 2)
    (list1, list2)
  }
  //test
  println(divide(List(1, 2, 3, 4, 5, 6)))
  println(divide(List(1, 2, 3, 4, 5)))
  println("agora sem splitAt que nao é recursivo")
  def divide1[T](list:List[T]): (List[T], List[T]) = {
    val split = list.length / 2
    divideAux(list, Nil, Nil, split)
  }

  def divideAux[T](list:List[T], list1:List[T], list2:List[T], c:Int): (List[T], List[T]) = {
    list match {
      case Nil => (list1, list2)
      case x::xs => if (c > 0) divideAux(xs, list1:+x, list2, c-1)
      else divideAux(xs, list1, list2:+x, c)
    }
  }

  println(divide1(List(1, 2, 3, 4, 5, 6)))
  println(divide1(List(1, 2, 3, 4, 5)))

}
