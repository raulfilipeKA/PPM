package Projeto2025

import Projeto2025.MyRandom

class Tarefas {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)
  type Stone = (Coord2D, Char)
  type lstOpenCoords = List[Coord2D] // (size**2 - this.len) == numero de jogadas (par P1, ímpar P2)
  
  
  object Turn extends Enumeration {
    type Turn = Value
    val B, W = Value
    
    def next(turn: Turn): Turn = turn match {
      case B => W
      case W => B
    }
    def startingTurn: Turn = {
      val turn = MyRandom.headOrTail
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
  def placeStone(board: Board, coord: Coord2D, value: Char): Board = {
    val (row, col) = coord
    if(board(row)(col) != '.') throw new IllegalStateException("Cell already occupied")
    value match {
      case 'B'|'W' => {
        board(row).updated(col, value)
        lst 
      }
      case 'E' => board(row).updated(col, value)
      //case 'E' => board(row).updated(col, '.') //so fazemos '.' para o print do tabuleiro
      case _ => throw new IllegalArgumentException("Invalid letter")
    }
    board.updated(row, board(row).updated(col, value))
  }

  //T1
  //lstOpenCoords sao as coordenadas livres (E)
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    if (lstOpenCoords.isEmpty) throw new IllegalArgumentException()
    rand.nextCoord(lstOpenCoords) // Apenas retorna a coordenada, faz se a validação depois na jogada
    // todo makeMove(lstOpenCoords, )
  }
  





}
