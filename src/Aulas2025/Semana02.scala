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
  
//  def divideList(list: List[Int], c: Int): (List[Int], List[Int]) = {
//    
//  }
//  
//  def divideListAux(list1: List[Int], list2: List[Int]): List[Int] = {
//    
//  }











  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
  val empty = List()

  def divide1(list: List[Int]): (List[Int], List[Int]) = {
    def helper(orig: List[Int], firstHalf: List[Int], count: Int): (List[Int], List[Int]) = orig match {
      case Nil => (firstHalf, Nil) // Se a lista original for vazia, retorna acumulador e lista vazia
      case _ if count == 0 => (firstHalf, orig) // Quando atingimos a metade, retorna as duas listas
      case x :: xs => helper(xs, firstHalf :+ x, count - 1) // Adicionamos à primeira metade
    }
    helper(list, Nil, list.length / 2)
  }
  println(divide1(List(1, 2, 3, 4)))

  def divide[A](list: List[A]): (List[A], List[A]) = {
    def helper(remaining: List[A], firstHalf: List[A], secondHalf: List[A], index: Int, mid: Int): (List[A], List[A]) = {
      remaining match {
        case Nil => (firstHalf.reverse, secondHalf.reverse)
        case head :: tail =>
          if (index < mid) helper(tail, head :: firstHalf, secondHalf, index + 1, mid)
          else helper(tail, firstHalf, head :: secondHalf, index + 1, mid)
      }
    }

    val mid = (list.length + 1) / 2
    helper(list, Nil, Nil, 0, mid)
  }



}
