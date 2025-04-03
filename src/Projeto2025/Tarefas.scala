package Projeto2025

import scala.util.Random
import Projeto2025.MyRandom

class Tarefas {

  type Board = List[List[Stone]]
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

  def whoseTurn(board: Board, lstOpenCoords: LstOpenCoords): Char = { //alterar caso se pretenda outro a iniciar
    if(board.length-lstOpenCoords.length %2 ==0) 'B'
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
  def placeStone(board: Board, coord: Coord2D, value: Char, lstOpenCoords: LstOpenCoords): (Board, LstOpenCoords) = {
    val (row, col) = coord
    if (getValueAt(board, coord) != 'E') throw new IllegalStateException("Cell already occupied")

    value match {
      case 'B' | 'W' | 'E' => {
        // Atualiza o board
        val updatedBoard = updateBoard(board, coord, value)
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
    if (lstOpenCoords.isEmpty) throw new IllegalStateException()
    rand.nextCoord(lstOpenCoords) // Apenas retorna a coordenada e o novo rand (ver implementação de nextCoord)
  }

  //T2
  def play(board:Board, player: Stone, coord:Coord2D, lstOpenCoords:List[Coord2D]):(Option[Board], List[Coord2D])={
    if (cellIsEmpty(board, coord)) {
      val value = whoseTurn(board, lstOpenCoords)
      val (updatedBoard, updatedCoords) = placeStone(board, coord, value, lstOpenCoords)
      if (updatedCoords.isEmpty) {
        // Se não houver mais coordenadas livres, o jogo termina
        (updatedBoard, updatedCoords)  //todo tipo Option??????
      } else {
        // Se ainda houver coordenadas livres, o jogo continua
        (Some(updatedBoard), updatedCoords)
      }
    } else {
      throw new IllegalStateException()
    }


  }

  //<html>Found:    (Tarefas.this.Board, Tarefas.this.LstOpenCoords)<br/>Required: (Option[Tarefas.this.Board], List[Tarefas.this.Coord2D])
  





}
