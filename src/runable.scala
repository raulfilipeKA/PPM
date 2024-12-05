import Semanas.*

object runable extends App{

  val ingredient = ("sugar", 25, "white")
  println(ingredient._1)
  println(ingredient._2.getClass)

  val semana1Instance = new semana1
  val result = semana1Instance.func2((1, 2), (3, 4))
  println({
    result
    (result._1+1,result._2+2)
  })
  println(11.0/2)
  println(11.0%2)

  def addThenMultiply(x: Int, y: Int)(multiplier: Int): Int =
    (x + y) * multiplier

  println(addThenMultiply(1, 2)(3)) // prints 9

  val addAndDouble: (Int, Int) => Int = (x, y) => addThenMultiply(x, y)(2)
  println(addAndDouble(1, 2)) // prints 6

  val addAndDouble1: (Int, Int) => Int = (x, y) => addThenMultiply(x, y)(2)
  println(addAndDouble1(1, 2))  // prints 6

  def name: String = System.getProperty("user.name")
  println("Hello, "+name +"!")

  val royal = (a:Int) => if(a>90) true else false
  println(royal(100))
  println(royal(80))
  royal(100)

  val numPairs = List((2, 5), (3, -7), (20, 56))
  println(numPairs.head) //??
  println(numPairs.length)
  println(numPairs.last)
  println(numPairs(0))
  println(numPairs.tail)

  def listOfDuplicates[A](x: A, length: Int): List[A] = {
    if (length < 1)  Nil
    else x :: listOfDuplicates(x, length - 1)
  }
  println(listOfDuplicates[Int](3, 4))  // List(3, 3, 3, 3)
  println(listOfDuplicates("La", 4))  // List(La, La, La, La)
  val bus :String = "Hello"
  def squareOf(x: Int): Int = x * x
}
