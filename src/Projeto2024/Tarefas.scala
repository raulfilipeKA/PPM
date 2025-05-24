package Projeto2024

import MyRandom.MyRandom

import scala.annotation.tailrec
import scala.io.Source  // Para ler arquivos de texto.

object Tarefas {
  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)

  // T1
  @tailrec
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    val (char, n_rand) = rand.nextChar
    if (char >= 'A' && char <= 'Z')
      return (char, n_rand)
    randomChar(n_rand)
  }

  // T2
  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {
    if (ValidaCoordenada(board, coord)) {
      // Atualiza a posição específica com a letra fornecida.
      val atualizaPosicao = board(coord._1).updated(coord._2, letter)
      // Atualiza a linha específica no tabuleiro.
      board.updated(coord._1, atualizaPosicao)
    } else {
      // Retorna o tabuleiro inalterado se a coordenada não for válida.
      board
    }
  }

  private def ValidaCoordenada(board: Board, coord: Coord2D): Boolean = {
    if (coord._1 >= 0 && coord._1 < board.length && coord._2 >= 0 && coord._2 < board.head.length)
      return true
    else
      return false
  }


  //T3

  // T3: Implementação do método setBoardWithWords para posicionar palavras no tabuleiro
  def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {
    @tailrec
    def helper(currentBoard: Board, remainingWords: List[String], remainingPositions: List[List[Coord2D]]): Board = {
      (remainingWords, remainingPositions) match {
        case (word :: ws, pos :: ps) =>
          // Combina cada letra da palavra com sua coordenada correspondente.
          // Por exemplo, para a palavra "CAT" e coordenadas [(0,0), (0,1), (0,2)],
          // word.zip(pos) cria uma lista de pares: [('C', (0,0)), ('A', (0,1)), ('T', (0,2))].
          val newBoard = word.zip(pos).foldLeft(currentBoard) {
            (acc, charPosPair) =>
              // Para cada par letra-coordenada, insere a letra no tabuleiro na posição especificada.
              fillOneCell(acc, charPosPair._1, charPosPair._2)
          }
          // Continua o processo para a próxima palavra e suas coordenadas.
          helper(newBoard, ws, ps)
        // Quando não houver mais palavras ou posições, retorna o tabuleiro atualizado.
        case (Nil, _) | (_, Nil) => currentBoard
      }
    }

    // Inicia o processo recursivo com o tabuleiro atual, a lista de palavras e suas coordenadas.
    helper(board, words, positions)
  }

  //FORMATAÇÃO EXEMPLO:
  //PROGRAMAR 3,3 2,3 1,2 1,1 2,1 3,0 3,1 4,2 4,3
  def readFile(filename: String): (List[String], List[List[(Int, Int)]]) = {
    // Lê todas as linhas do arquivo para uma lista.
    val lines = Source.fromFile(filename).getLines().toList

    // Separa as palavras das suas coordenadas.
    val wordsAndCoords = lines.map { line =>
      // Divide a linha em partes: a primeira é a palavra, o resto são as coordenadas.
      val parts = line.split(" ")
      val word = parts.head
      val coords = parts.tail.map { coord =>
        // Para cada coordenada, divide em x e y, e converte para inteiros.
        val Array(x, y) = coord.split(",").map(_.toInt)
        (x, y) // Usa uma tupla diretamente em vez da case class.
      }.toList

      (word, coords)
    }

    // Separa as palavras das listas de coordenadas em duas listas separadas.
    val (words, coordLists) = wordsAndCoords.unzip

    (words, coordLists)
  }

  // T4:
  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = {
    @tailrec
    def fillRow(row: List[Char], rand: MyRandom, accRow: List[Char]): (List[Char], MyRandom) = row match {
      // Quando não houver mais caracteres na linha, retorna a linha acumulada e o estado atual do gerador
      case Nil => (accRow, rand)
      // Se o caractere atual for um espaço vazio
      case '_' :: tail =>
        // Gera um novo caractere
        val (char, newRand) = f(rand)
        // Continua para o restante da linha com o novo caractere adicionado
        fillRow(tail, newRand, accRow :+ char)
      // Se o caractere atual não for um espaço vazio
      case head :: tail =>
        // Mantém o caractere e continua para o restante da linha
        fillRow(tail, rand, accRow :+ head)
    }

    @tailrec
    def fillBoard(rows: List[List[Char]], rand: MyRandom, accBoard: Board): (Board, MyRandom) = rows match {
     // Quando não houver mais linhas, retorna o tabuleiro acumulado e o estado atual do gerador
     case Nil => (accBoard, rand)
      case head :: tail =>
        // Preenche uma linha
        val (newRow, newRand) = fillRow(head, rand, List.empty)
        // Continua para o restante do tabuleiro com a linha atualizada adicionada
        fillBoard(tail, newRand, accBoard :+ newRow)
    }

    fillBoard(board, r, List.empty)
  }

  // T5: Implementação do método play
  def play(board: Board, word: String, startCoord: Coord2D, direction: Direction.Value, allowedWords: List[String]): Boolean = {
    // Mapeia as direções para deslocamentos
    val offsets = Map(
      Direction.North -> (-1, 0),
      Direction.South -> (1, 0),
      Direction.East -> (0, 1),
      Direction.West -> (0, -1),
      Direction.NorthEast -> (-1, 1),
      Direction.NorthWest -> (-1, -1),
      Direction.SouthEast -> (1, 1),
      Direction.SouthWest -> (1, -1)
    )

    // Direções possíveis para continuar a busca após a primeira letra
    val allDirections = offsets.values.toList
    if (!allowedWords.contains(word)) return false

    // Verifica se a próxima letra da palavra corresponde ao tabuleiro, começando com a direção dada
    def checkNext(coord: Coord2D, remainingWord: String, maybeDirection: Option[(Int, Int)]): Boolean = {
      if (remainingWord.isEmpty) true // Se todas as letras corresponderem, a palavra existe
      else {
        val (row, col) = coord
        if (row < 0 || row >= board.length || col < 0 || col >= board.head.length) false // Se estiver fora dos limites do tabuleiro
        else if (board(row)(col) != remainingWord.head) false // Se a letra não corresponder
        else {
          // Define as direções a seguir baseado na disponibilidade de maybeDirection
          val nextDirections = if (maybeDirection.isDefined) {
            List(maybeDirection.get) // Usa a direção especificada
          } else {
            allDirections // Ou todas as direções possíveis se nenhuma específica foi fornecida
          }
          nextDirections.exists { case (dRow, dCol) =>
            val nextCoord = (row + dRow, col + dCol)
            checkNext(nextCoord, remainingWord.tail, None) // Nenhuma direção específica para letras subsequentes
          }
        }
      }
    }
    // Inicie a verificação da primeira letra com a direção especificada
    checkNext(startCoord, word, offsets.get(direction))
  }

  def isValidCoord(board: Board, coord: Coord2D): Boolean =
    coord._1 >= 0 && coord._1 < board.length && coord._2 >= 0 && coord._2 < board.head.length

  //T6:
  // T6: Extração de palavras do tabuleiro
  def playAndUpdate(gameState: GameState, word: String, startCoord: Coord2D, direction: Direction.Value, wordCoords: List[Coord2D]): GameState = {
    if (play(gameState.board, word, startCoord, direction, gameState.allowedWords)) {
      //println(s"Adding positions for found word: $word at positions $wordCoords")
      val newFoundWords = gameState.foundWords + word
      val newScore = gameState.score + 10
      gameState.copy(foundWords = newFoundWords, score = newScore, foundPositions = gameState.foundPositions ++ wordCoords.toSet)
    } else gameState
  }

  def checkBoard(board: Board, words: List[String], positions: List[List[Coord2D]]): Boolean = {
    words.zip(positions).forall { case (word, posList) =>
      posList.map { pos =>
        if (isValidCoord(board, pos)) board(pos._1)(pos._2) else '_'
      }.mkString == word
    }
  }

  // case class GameState()
}