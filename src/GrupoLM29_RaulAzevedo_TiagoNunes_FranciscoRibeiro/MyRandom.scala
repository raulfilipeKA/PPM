package GrupoLM29_RaulAzevedo_TiagoNunes_FranciscoRibeiro
import scala.util.Random

case class MyRandom(seed: Long) extends RandomWithState {
  def nextInt(): (Int, RandomWithState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }

  def nextInt(n: Int): (Int, RandomWithState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val nn = ((newSeed >>> 16).toInt) % n
    (if(nn < 0) -nn else nn, nextRandom)
  }

  // Alterado para garantir que o tipo retornado seja MyRandom
  def nextCoord(lstOpenCoords: List[(Int, Int)]): ((Int, Int), MyRandom) = {
    if (lstOpenCoords.isEmpty) throw new IllegalStateException()

    val (index, newRand) = nextInt(lstOpenCoords.length) // Obtemos o índice aleatório
    val coord = lstOpenCoords(index) // Obtém a coordenada correspondente ao índice

    (coord, newRand.asInstanceOf[MyRandom]) // Forçando o tipo para MyRandom
  }
}

trait RandomWithState {
  def nextInt: (Int, RandomWithState)
  def nextInt(n: Int): (Int, RandomWithState)
}
