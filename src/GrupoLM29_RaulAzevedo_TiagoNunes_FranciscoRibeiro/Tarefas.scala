package GrupoLM29_RaulAzevedo_TiagoNunes_FranciscoRibeiro

import scala.util.Random
import GrupoLM29_RaulAzevedo_TiagoNunes_FranciscoRibeiro.MyRandom

import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationLong
import scala.concurrent.{Await, Future}

class Tarefas {

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int) //(row, column)
  type Stone = (Coord2D, Char)
  type LstOpenCoords = List[Coord2D] // (size**2 - this.len) == numero de jogadas (par P1, ímpar P2)
  type History = List[(Board, LstOpenCoords)] //historico de jogadas


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

  //Aux function to update the value at a specific coordinate
  def updateBoard(board: Board, coord: Coord2D, value: Char): Board = {
    val (row, col) = coord
    board.updated(row, board(row).updated(col, (coord, value)))
  }

  // Function to remove an item from a list
  def removeItem[T](list: List[T], item: T): List[T] = list match {
    case Nil => Nil
    case head :: tail if head == item => removeItem(tail, item)
    case head :: tail => head :: removeItem(tail, item)
  }

  def containsInList[T](list: List[T], item: T): Boolean = {
    list match {
      case Nil => false
      case head :: tail if head == item => true
      case _ :: tail => containsInList(tail, item)
    }
  }

  //T1
  //lstOpenCoords sao as coordenadas livres (E)
  // na funcao que usar randomMove, vou ter que criar uma instância de MyRandom para passar
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    if (lstOpenCoords.isEmpty) throw new IllegalStateException()
    rand.nextCoord(lstOpenCoords) // Apenas retorna a coordenada e o novo rand (ver implementação de nextCoord)
  }


  //T2
  // Auxiliar function to set the value at a specific coordinate
  def placeStone(board: Board, coord: Coord2D, lstOpenCoords: LstOpenCoords): (Board, LstOpenCoords) = {
    if (getValueAt(board, coord) != 'E') throw new IllegalStateException("Cell already occupied")
    val turn = whoseTurn(board, lstOpenCoords)

    turn match { //serve para saber se se joga B ou W
      case 'B' | 'W' => { //nao vamos jogar Empty e vamos implementar isso num metodo para verificar se "comemos" alguma pedra
        // Atualiza o board
        val updatedBoard = updateBoard(board, coord, turn)
        // Remove a coordenada da lista de coordenadas livres
        val updatedCoords = removeItem(lstOpenCoords, coord)
        (updatedBoard, updatedCoords) // Devolve o board atualizado e a lista de coordenadas livres
      }
      case _ => throw new IllegalArgumentException("Invalid letter")
    }
  }

  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    if (cellIsEmpty(board, coord)) { // nao me parece necessario o player dado que já passamos a coordenada, ter os dois é redundante

      if (containsInList(lstOpenCoords, coord)) {
        val (updatedBoard, updatedLstOpenCoords) = placeStone(board, coord, lstOpenCoords)

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



  //T3
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D], history: History,
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D], History) = {

    val (coord, newRand) = f(lstOpenCoords, r) // Obtemos a coordenada aleatória e o novo gerador de números aleatórios
    val (updatedBoard, updatedLstOpenCoords, updatedHistory) = playWithHistory(board, player, coord, lstOpenCoords, history) // Coloca a pedra no tabuleiro (reutiliza a funcao play que ja faz validacoes necessarias para jogar)

    (updatedBoard.get, newRand, updatedLstOpenCoords, updatedHistory) // Retorna o tabuleiro atualizado, o novo gerador de números aleatórios e a lista de coordenadas livres
    //updatedBoard.get vai buscar o tabuleiro ao Option
  }


  //T4
  def drawBoard(side: Int): Board = { // Cria um tabuleiro vazio de tamanho side x side
    def createRow(row: Int, col: Int): List[Stone] = {
      if (col >= side) Nil
      else ((row, col), 'E') :: createRow(row, col + 1) // Cria uma nova pedra com coordenadas (row, col) e valor 'E' (vazia)
    }

    def createBoard(row: Int): Board = {
      if (row >= side) Nil
      else createRow(row, 0) :: createBoard(row + 1)
    }

    createBoard(0)
  }


  def printBoard(board: Board): Unit = {
    def printRow(row: List[Stone]): Unit = { //funcao auxiliar para imprimir uma linha do tabuleiro
      row match {
        case Nil => println() // fim da linha
        case (_, value) :: tail =>
          val charToPrint = value match {
            case 'E' => '.'
            case c => c
          }
          print(charToPrint + " ")
          printRow(tail) // chamada recursiva para imprimir o resto da linha
      }
    }

    board match { // vai fazendo print linha a linha
      case Nil => ()
      case head :: tail =>
        printRow(head)
        printBoard(tail)
    }
  }

  //T5
  def adjacentCoords(coord: Coord2D, size: Int): List[Coord2D] = { //Devolve Lista de coordenadas adjacentes válidas
    val (row, col) = coord
    List((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)) //Da lista de coordenandas adjacentes
      .filter { case (x, y) => x >= 0 && x < size && y >= 0 && y < size } // Filtrar para ver quais coordenandas válidas
  }

  def findGroup(board: Board, coord: Coord2D, targetStone: Char, visited: Set[Coord2D]): Set[Coord2D] = { // Encontra todas as peças conectadas a partir de uma coordenada
    if (getValueAt(board, coord) != targetStone || visited.contains(coord)) visited
    else {
      val newVisited = visited + coord //adiciona a coordenada atual ao conjunto de visitadas
      adjacentCoords(coord, board.length).foldLeft(newVisited) { (accumulatedVisited, next) => //Para todas as corrdenadas adjacentes verifcar se pertencem ao grupo
        findGroup(board, next, targetStone, accumulatedVisited)
      }
    }
  }

  def groupHasLiberty(board: Board, group: Set[Coord2D]): Boolean = { //retorna true caso exista uma coordenada adjacente de uma peça pertencente ao grupo
    group.exists { coord => //exists, expressão lambda caso aja pelo menos uma que verifique essa condição
      adjacentCoords(coord, board.length).exists { adj => //por cada coordenada group -> adjacentes para ver se é 'E'
        getValueAt(board, adj) == 'E'
      }
    }
  }

  def removeGroupFromBoard (board: Board, group: Set[Coord2D]) : Board = { //remover grupo de peças da board 'capturadas'
    group.foldLeft(board){ (newBoard, coord) =>
      updateBoard(newBoard,coord,'E')
    }//futuramente atualizar a função para ++peças capturadas
  }

  def captureGroupStones(coord: Coord2D, board: Board, player: Stone): (Board, Int) ={ //titulo self explainatory

    val opponnent = player._2 match{ //definir oponente
      case 'B' => 'W'
      case 'W' => 'B'
    }

    val adjacentOpponentCoords = adjacentCoords(coord, board.length) //encontra coordenadas de peças oponentes
      .filter(adjacentCoord => getValueAt(board, adjacentCoord) == opponnent)

    //Grupos de peças do player opponent
    val opponentsGroups = adjacentOpponentCoords.map(adjacentCoord => findGroup(board,adjacentCoord,opponnent,Set.empty)).toSet

    val (finalBoard, totalCaptured) = opponentsGroups.foldLeft((board, 0)) { //por cada grupo encontrado (normalmente só 1)
      case ((currentBoard, count), group) =>                                 //percorrer grupo com valores iniciais (board,0)
        if (groupHasLiberty(currentBoard, group)) (currentBoard, count)      //grupo tiver liberdade ent continua
        else (removeGroupFromBoard(currentBoard, group), count + group.size) //se tiver surrounded as peças são capturadas
    }
    (finalBoard, totalCaptured) //retornar a board final e as peças capturadas naquela jogada
  }


  //T6

  //T7
  def playWithTimeout(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D], history: History, rand: MyRandom, timer: Long): (Board, MyRandom, List[Coord2D], History) = {
    val userMove = Future {   //representa a jogada futura do utilizador e é corrida de forma assíncrona, de modo a que o timer nao bloqueie a aplicação
      playWithHistory(board, player, coord, lstOpenCoords, history)
    }

    try {
      val result = Await.result(userMove, timer.millis) // espera pelo resultado da jogada do utilizador até ao tempo limite especificado
      result match {
        case (Some(updatedBoard), updatedLstOpenCoords, updatedHistory) => //se a jogada for válida
          (updatedBoard, rand, updatedLstOpenCoords, updatedHistory)
        case _ =>                                                             //se fier uma jogada inválida dentro do tempo
          playRandomly(board, rand, player, lstOpenCoords, history, randomMove)
      }
    } catch {
      case _: TimeoutException =>  // se a jogada do utilizador exceder o tempo limite
        playRandomly(board, rand, player, lstOpenCoords, history, randomMove)
    }
  }


  //funcao auxiliar para jogar com histórico por causa do undo
  def playWithHistory(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D], history: History): (Option[Board], List[Coord2D], History) = {
    play(board, player, coord, lstOpenCoords) match {
      case (Some(newBoard), updatedLstOpenCoords) =>
        val newHistory = (newBoard, updatedLstOpenCoords) :: history
        (Some(newBoard), updatedLstOpenCoords, newHistory)
      case (None, _) =>
        (None, lstOpenCoords, history)
    }
  }

  def undo(history: List[(Board, LstOpenCoords)]): (Option[(Board, LstOpenCoords)], List[(Board, LstOpenCoords)]) = {
    history match {
      case _ :: previousState :: rest =>
        (Some(previousState), previousState :: rest) //
      case _ =>
        (None, history) // não há estado anterior
    }
  }



  //T8

  //T9

}
