package Projeto2025

import scala.util.Random

class Tarefas {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)
  type Stone = (Coord2D, Char)
  type LstOpenCoords = List[Coord2D] // (size**2 - this.len) == numero de jogadas (par P1, ímpar P2)
  
  
  object Turn extends Enumeration {
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

  def getValueAt(board: Board, coord: Coord2D): Char = {
    val (row, col) = coord
    board(row)(col)
  }
  
  def cellIsEmpty(board: Board, coord: Coord2D): Boolean = {
    getValueAt(board, coord) == 'E'
  }

  // Function to set the value at a specific coordinate
  def placeStone(board: Board, coord: Coord2D, value: Char, lstOpenCoords: LstOpenCoords): (Board, LstOpenCoords) = {
    val (row, col) = coord
    if (board(row)(col) != 'E') throw new IllegalStateException("Cell already occupied")

    value match {
      case 'B' | 'W' | 'E' => {
        // Atualiza o board
        val updatedBoard = board.updated(row, board(row).updated(col, value))
        // Remove a coordenada da lista de coordenadas livres
        val updatedCoords = removeItem(lstOpenCoords, coord)
        (updatedBoard, updatedCoords)  // Devolve o board atualizado e a lista de coordenadas livres
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
    if (lstOpenCoords.isEmpty) throw new IllegalArgumentException()
    rand.nextCoord(lstOpenCoords) // Apenas retorna a coordenada, faz se a validação depois na jogada
    // todo makeMove(lstOpenCoords, )
  }
  





}
