package GrupoLM29_RaulAzevedo_TiagoNunes_FranciscoCorreia_FranciscoRibeiro

import scala.util.Random
import GrupoLM29_RaulAzevedo_TiagoNunes_FranciscoCorreia_FranciscoRibeiro.MyRandom

class Tarefas {

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int) //(row, column)
  type Stone = (Coord2D, Char)
  type LstOpenCoords = List[Coord2D] // (size**2 - this.len) == numero de jogadas (par P1, ímpar P2)


  object Turn extends Enumeration {  //ja nao vamos utilizar porque a funcao whoseTurn ja faz o calculo automaticamente de maneira simples
    type Turn = Value
    val B, W = Value

    def next(turn: Turn): Turn = turn match {
      case B => W
      case W => B
    }

    def startingTurn: Turn = {
      val turn = Random.nextBoolean()
      turn match {
        case true => B
        case _ => W
      }
    }
  }

  def whoseTurn(board: Board, lstOpenCoords: LstOpenCoords): Char = { //alterar caso se pretenda outro a iniciar
    if (((board.length * board(0).length) - lstOpenCoords.length) % 2 == 0) 'B'
    else 'W'
  }

  def getValueAt(board: Board, coord: Coord2D): Char = {
    val (row, col) = coord
    //val stone = board(row)(col)
    //stone._2
    board(row)(col)._2
  }

  def cellIsEmpty(board: Board, coord: Coord2D): Boolean = {
    getValueAt(board, coord) == 'E'
  }

  def updateBoard(board: Board, coord: Coord2D, value: Char): Board = {
    val (row, col) = coord
    board.updated(row, board(row).updated(col, (coord, value)))
  }

  // Function to set the value at a specific coordinate
  def placeStone(board: Board, coord: Coord2D, lstOpenCoords: LstOpenCoords): (Board, LstOpenCoords) = {
    if (getValueAt(board, coord) != 'E') throw new IllegalStateException("Cell already occupied")
    val turn = whoseTurn(board, lstOpenCoords)
    turn match {
      case 'B' | 'W' => { //nao vamos jogar Empty e vamos implementar isso num metodo para verificar se "comemos" alguma pedra
        (updateBoard(board, coord, turn), removeItem(lstOpenCoords, coord)) // Devolve o board atualizado e a lista de coordenadas livres
      }
      case _ => throw new IllegalArgumentException("Invalid letter")
    }
  }


  // Function to remove an item from a list
  def removeItem[T](list: List[T], item: T): List[T] = list match {
    case Nil => Nil
    case head :: tail if head == item => removeItem(tail, item)
    case head :: tail => head :: removeItem(tail, item)
  }


  //T1
  //lstOpenCoords sao as coordenadas livres (E)
  // na funcao que usar randomMove, vou ter que criar uma instância de MyRandom para passar
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    if (lstOpenCoords.isEmpty) throw new IllegalStateException()
    rand.nextCoord(lstOpenCoords) // Apenas retorna a coordenada e o novo rand (ver implementação de nextCoord)
  }
//todo fazer update da lstOpenCoords depois de jogar -> mas so se faz numa funcao ( na place stone que é mais genérica)
  //T2
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    if (cellIsEmpty(board, coord)) {           // nao me parece necessario o player dado que já passamos a coordenada, ter os dois é redundante

      if (containsInList(lstOpenCoords, coord)) {
        val (updatedBoard, updatedLstOpenCoords) = placeStone(board, coord, lstOpenCoords)
        //verificar se o jogo terminou
        if (updatedLstOpenCoords.isEmpty) {
          (Some(updatedBoard), updatedLstOpenCoords) // tipo Option?????? -> instance of Some or none
        } else {
          (Some(updatedBoard), updatedLstOpenCoords) // retorna o tabuleiro atualizado e a lista de coordenadas livres
        }
      } else {
        (None, lstOpenCoords) // retorna None como pedido no enunciado e a lista de coordenadas livres
      }
    } else {
      (None, lstOpenCoords)
    }
  }

  def containsInList[T](list: List[T], item: T): Boolean = {
    list match {
      case Nil => false
      case head :: tail if head == item => true
      case _ :: tail => containsInList(tail, item)
    }
  }

  //T3
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) = {

    val (coord, newRand) = f(lstOpenCoords, r) // Obtemos a coordenada aleatória e o novo gerador de números aleatórios
    val (updatedBoard, updatedLstOpenCoords) = play(board, player, coord, lstOpenCoords) // Coloca a pedra no tabuleiro (reutiliza a funcao play que ja faz validacoes necessarias para jogar)

    (updatedBoard.get, newRand, updatedLstOpenCoords) // Retorna o tabuleiro atualizado, o novo gerador de números aleatórios e a lista de coordenadas livres
    //updatedBoard.get vai buscar o tabuleiro ao Option
  }

  val rand = new MyRandom(42)
  val tabuleiro = drawBoard(3)
  val livres = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
  val player: Stone = ((0, 0), 'B') // só importa o 'B'

  val resultado = playRandomly(tabuleiro, rand, player, livres, randomMove)


  def drawBoard(side: Int): Board = {
    def createRow(row: Int, col: Int): List[Stone] = {
      if (col >= side) Nil
      else ((row, col), 'E') :: createRow(row, col + 1)  // Cria uma nova pedra com coordenadas (row, col) e valor 'E' (vazia)
    }
    def createBoard(row: Int): Board = {
      if (row >= side) Nil
      else createRow(row, 0) :: createBoard(row + 1)
    }
    createBoard(0)
  }


  def printBoard(board: Board): Unit = {
    def printRow(row: List[Stone]): Unit = {
      row match {
        case Nil => println() // fim da linha
        case (_, value) :: tail =>
          val charToPrint = value match {
            case 'E' => '.'
            case c => c
          }
          print(charToPrint + " ")
          printRow(tail)
      }
    }

    board match {
      case Nil => ()
      case head :: tail =>
        printRow(head)
        printBoard(tail)
    }
  }

  def adjacentCoords(coord: Coord2D, size: Int): List[Coord2D] = { //Devolve Lista de coordenadas adjacentes válidas
    val (row, col) = coord
    List((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)) //Da lista de coordenandas adjacentes
      .filter { case (x, y) => x >= 0 && x < size && y >= 0 && y < size } // Filtrar para ver quais coordenandas válidas
  }

  def findGroup(board: Board, coord: Coord2D, target: Char, visited: Set[Coord2D]): Set[Coord2D] = {
    if (getValueAt(board, coord) != target || visited.contains(coord)) visited
    else {
      val newVisited = visited + coord //adiciona a coordenada atual ao conjunto de visitadas
      adjacentCoords(coord, board.length).foldLeft(newVisited) { (acc, next) => //Para todas as corrdenadas adjacentes verifcar se pertencem ao grupo
        findGroup(board, next, target, acc)
      }
    }
  }

  //T5
//  def captureGroupStones(board: Board, player: Stone): (Board, Int) ={
//
//  }


}
