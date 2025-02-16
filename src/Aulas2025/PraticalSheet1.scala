package Aulas2025

object PraticalSheet1 extends App {

  //singleton object
  object Fun {
    def func1(x: Double, y: Int) = x + (70 * y)

    def ex(a: Double) = 50 * a
  }

//  >: type Fun
//.func1(_, _)
//  >: t Fun.ex(_)

  println(Fun.func1(1,2))
  println(Fun.ex (1))

  println(Fun.func1(Fun.ex(10.2), 1))
  
  //a
  def a(x: Int, y: Int) = ((x+y), (x*y))
  println(a(1,2))
  //b
  def b(x:Int, y:Int, z:Int) = {
    if(x>y && x>z) {
      if(y>z) (x,y)
      else (x,z)
    }
    else if(y>x && y>z) {
      if(x>z) (y,x)
      else (y,z)
    }
    else {
      if(x>y) (z,x)
      else (z,y)
    }
  }
  println(b(1,2,3))
  println(b(1,3,2))
  println(b(2,1,3))
  println(b(-1,-3,0))

  //c
  def c(a: Int, b: Int, c: Int): Boolean = {
    val l = PraticalSheet1.b(a, b, c) //temos que meter PraticalSheet1. por conta da ambiguidade do nome do param e da funcao
    val la = l._1 - l._2
    if (la < a && la < b && la < c) (true)
    else (false)
  }
  println(c(6, 6, 6))
  println(c(1, 2, 3))
  println(1, 2, 3)
  println(c(22, 32, 12))

  //d
  def abrev(x: String): String = {
    val l = x.split(" ")
    //l(0) + " " + l(l.length-1)
    l.head + " " + l.last
  }
  println(abrev("Joao Manu Silva"))


  //factorial
  def fact(n: Int): Int = if (n == 0) 1 else n * fact(n - 1)

  //3a
  def a3(x: Int, y: Int): Int = {
    if(y==0) 1
    else if(x == 1 || y==1) x
    else x*a3(x, y-1)
  }
  println(a3(2,3))

  //3b
  def b3(list:List[Int]) = (list(0), list(list.length-1))

  //3c






}
