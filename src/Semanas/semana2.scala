package Semanas

object semana2 extends App{
  val x = 1
  val li1= 1::(2::(3::(4::Nil)))
  val li2 = x::(2::(3::(4::Nil)))
  val li3 = x::(2::List())
  val li4 = x::li1
  println(li1)
  println(li2)
  println(li3)
  println(li4)
  println("here")

  print((1 until 10).toSet)
  val a = (1 to 5).toSet
  val c = (1 to 5).toSet
  val d = (1 to 5).toSet

  val b = (1 to 5).toList
  println(b++a)
  println(a)
  println(d)
  println(c)
println("")

println(met())

}

def length[E](list: List[E]): Int = {
  if (list.isEmpty) 0
  else 1 + length(list.tail)
}

def num(x: Int) = x match{
  case _ if(x>0) => println("positive")
  case _ if(x<0) => println("negative")
  case _ if(x==0) => println("zero")
}

def met() = {
  val a = 1
  val x = 1
  val y = 1
  x eq y
}

def isort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case x :: xs1 => insert(x, isort(xs1))
}
def insert (x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => List(x) //if the list is empty, then x is the head
  case y :: ys => if (x <= y) x :: xs //if x is less than or equal to y, then x is the new head
  else y :: insert(x, ys) //if x is greater than y, then y is the new head
}