
package Projeto2024

object MyRandom{
  trait Random {
    def nextInt: (Int, Random)
    def nextChar: (Char, Random)
  }
  case class MyRandom(seed: Long) extends Random {
    def nextInt: (Int, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) &
        0xFFFFFFFFFFFFL
      val nextRandom = MyRandom(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRandom)
    }
    def nextChar: (Char, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) &
        0xFFFFFFFFFFFFL
      val nextRandom = MyRandom(newSeed)
      val n = (newSeed >>> 16).toChar
      (n, nextRandom)
    }
  }

}

