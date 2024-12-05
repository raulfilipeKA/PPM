object Pratica extends App{
  def sumMultiply(a: Int, b: Int): (Int, Int) = { // 1a
    val c = a + b
    val d = a * b
    return (c, d)
  }

  def twoBiggestOf3(a: Int, b: Int, c: Int): (Int, Int) = {
    if (a >= b && a >= c) { //a
      if (b >= c) {
        return (a, b)
      } else {
        return (a, c)
      }
    }
    if (b >= a && b >= c) { //b
      if (a >= c) {
        return (b, a)
      }
      else {
        return (b, c)
      }
    }
    else { //c
      if (a >= b) {
        return (c, a)
      }
      else {
        return (c, b)
      }
    }
  }

  def threeInDescendingOrder(a: Int, b: Int, c: Int): (Int, Int, Int) = {
    if (a >= b && a >= c) { //a
      if (b >= c) {
        return (a, b, c)
      } else {
        return (a, c, b)
      }
    }
    if (b >= a && b >= c) { //b
      if (a >= c) {
        return (b, a, c)
      }
      else {
        return (b, c, a)
      }
    }
    else { //c
      if (a >= b) {
        return (c, a, b)
      }
      else {
        return (c, b, a)
      }
    }
  }

  def isTriangle(a: Double, b: Double, c: Double): Boolean = {
    if (a + b > c && b + c > a && a + c > a) true
    else false
  }

  def abrev(a: String): Any = {
    val vname = a.split(" ")
    val name1 = vname(0)
    val name2 = vname(vname.length - 1)

    print(name1 + " " + name2)

  }

  def xPy(x: Int, y: Int): Int = {
    if (y == 0) return 1
    else x* xPy(x, y-1)
  }

  def pairList(x: List[Any]): (Any, Any) = {
     (x.head, x(x.length-1))
  }

  def listLen(x: List[Any]): (List[Any], Int)={
    (x, x.length-1)
  }

  def listAverage(x: List[Double]): Double ={
    (sum(x) / x.length)
  }
  def sum(x:List[Double]): Double = x match {
    case Nil => 0
    case y::ys => y+sum(ys)
  }
  def transf(x: List[Any]):List[Any] = x match {
    case Nil => Nil
    case y::Nil => List(y)
    case y::z::ys => z::y::transf(ys)
  }
  def multiplyListElems(x:List[Double]): Double ={
    x.reduceLeft(_*_)
  }
  def headToEnd(x:List[Double]): List[Double] = x match {
    case Nil => Nil
    case y::Nil => y::Nil
    case y::ys => ys :+ y
  }
  def concat(x:List[Any], y: List[Any]): List[Any] = y match {
    case Nil => x
    case ys::Nil => x:+ys
    case head::ys => concat(x:+head, ys)
  }
  def concat1(x:List[Any], y: List[Any]): List[Any] = {
    x++y
  }
  def sumEl(x: List[(Int, Int)], i:Int = 0, sum: Int = 0): Int = x match{
    case Nil => sum
    case head::tail if(i==2 || i==4) =>
      sumEl(tail, i + 1, sum + head._1 + head._2)
    case _::tail => sumEl(tail, i + 1, sum)
  }

  def sumEl1(xs: List[(Int, Int)], i: Int = 0, sum: Int = 0): Int = xs match {
    case Nil => sum
    case h :: t =>
      if (i == 2 || i == 4)
        sumEl1(t, i + 1, sum + h._1 + h._2)
      else
        sumEl1(t, i + 1, sum)
  }



}