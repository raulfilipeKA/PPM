package Projeto2025

import scala.util.Random


class MyRandom extends Random{
  
  def headOrTail: Boolean = nextBoolean()

  def nextCoord(lstOpenCoords: List[(Int, Int)]): ((Int, Int), MyRandom) = {
    val randomIndex = nextInt(lstOpenCoords.length)
    (lstOpenCoords(randomIndex), this)
  }

}
