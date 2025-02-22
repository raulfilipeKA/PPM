package Aulas2025

object Semana01 extends App{  //extends App is used to run the object as a program
                             //object instead of class is used to create a singleton object
  def ex1(a:Int) ={
    a+1
  }

  def ex2(x:Int) ={
   val y = 1;
    (x, y, "oal")
  }

  def ex21(x:Int): (Int, Int, String) ={
    val y = 1;
    (x, y, "oal")

  }
  println(ex1(1))
  println(ex2(1))
  println(ex21(1))


}


