package Projeto2024

import MyRandom.MyRandom
import Tarefas._

import scala.io.StdIn.readLine
import scala.util.Random // Importa as funções definidas no objeto Tarefas.

//case class GameState(board: Board, random: MyRandom)
case class GameState( // Estado do jogo
                      board: List[List[Char]], // Representação do tabuleiro
                      random: MyRandom, // Gerador de números aleatórios customizado
                      allowedWords: List[String], // Palavras permitidas no jogo
                      foundWords: Set[String] = Set(), // Palavras encontradas
                      foundPositions: Set[(Int, Int)] = Set(), // Posições das palavras encontradas
                      startTime: Long, // Tempo de início
                      endTime: Long = 0L, // Tempo de fim (não usado até o jogo terminar)
                      score: Int = 0 // Pontuação atual
                    )


object SopaDeLetrasUtils { // Objeto utilitário para funções auxiliares
  def showPrompt(): Unit = {  // Exibe as opções disponíveis para o usuário
    println("\nEscolha uma opção:") // Exibe as opções disponíveis para o usuário
    println("(p)alavra para selecionar")
    println("(r)einiciar")
    println("(q)uit")
    print("O que você gostaria de fazer? \n")
  }

  def getUserInput: String = readLine.trim.toUpperCase

  def printGameState(gameState: GameState): Unit = {
    println("\nEstado atual do tabuleiro:")
    printBoard(gameState.board, gameState.foundPositions)
    println(s"Pontuação atual: ${gameState.score}")
    val found = gameState.allowedWords.diff(gameState.foundWords.toList)
    println(s"Palavras restantes: ${found.mkString(", ")}")
    val currentTime = System.currentTimeMillis()
    val timeElapsed = (currentTime - gameState.startTime) / 1000
    println(s"Tempo de jogo: $timeElapsed segundos")
  }

  def printBoard(board: List[List[Char]], foundPositions: Set[(Int, Int)]): Unit = {
    //println(s"Found positions: $foundPositions")
    board.indices.foreach { i => // Itera sobre as linhas do tabuleiro
      board(i).indices.foreach { j => // Itera sobre as colunas do tabuleiro
        if (foundPositions.contains((i, j))) { // Verifica se a posição atual foi encontrada
          print(s"\u001B[32m${board(i)(j)}\u001B[0m ") // Verde para letras encontradas
        } else {
          print(s"${board(i)(j)} ") // Imprime a letra normalmente
        }
      }
      println() // Nova linha após cada linha do tabuleiro
    }
  }

  def printGameOver(): Unit = {
    println("\n=== FIM DE JOGO ===")  // Exibe a mensagem de fim de jogo
  }

  // Converte a entrada do usuário para a direção correspondente
  def getDirectionFromUserInput(input: String): Option[Direction.Value] = input match {
    case "N"  => Some(Direction.North) // Retorna a direção correspondente
    case "S"  => Some(Direction.South)
    case "E"  => Some(Direction.East)
    case "W"  => Some(Direction.West)
    case "NE" => Some(Direction.NorthEast)
    case "NW" => Some(Direction.NorthWest)
    case "SE" => Some(Direction.SouthEast)
    case "SW" => Some(Direction.SouthWest)
    case _    => None // Retorna None se a direção não for reconhecida
  }
}

object SopaDeLetrasGame extends App { // Objeto principal do jogo
  val initialSeed = 42 // Semente inicial para gerar números aleatórios
  val filePath = "Words.txt" // Caminho do arquivo de palavras

  // Leitura de palavras do arquivo
  val (wordsFromFile, coordsFromFile) = readFile(filePath)  // Lê as palavras e suas coordenadas do arquivo

  val positions: List[(String, List[(Int, Int)])] = wordsFromFile.zip(coordsFromFile) // Combina as palavras e suas coordenadas

  // Estado inicial do jogo
  val gameState = initializeGameState(initialSeed, wordsFromFile, positions.map(_._2)) // Inicializa o estado do jogo

  val coordLists: List[List[Coord2D]] = positions.map(_._2).asInstanceOf[List[List[Coord2D]]]
  if (Tarefas.checkBoard(gameState.board, wordsFromFile, coordLists)) {  // Verifica se o tabuleiro é válido antes de iniciar o jogo
    println("Tabuleiro Válido")
    gameLoop(gameState)
  } else {
    println("Tabuleiro Invalido: algumas palavras não foram localizadas corretamente")
  }

  // Método para inicializar o GameState com palavras de um arquivo
  def initializeGameState(seed: Long, allowedWords: List[String], positions: List[List[Coord2D]]): GameState = {
    val emptyBoard: Board = List.fill(5)(List.fill(5)('_')) // Tabuleiro vazio 5x5
    val newBoard = setBoardWithWords(emptyBoard, allowedWords, positions) // Define o tabuleiro com as palavras
    val (completedBoard, newRandom) = completeBoardRandomly(newBoard, MyRandom(seed), randomChar) // Completa o tabuleiro com letras aleatórias
    val startTime = System.currentTimeMillis() // Tempo de início do jogo

    GameState(completedBoard, newRandom, allowedWords, Set.empty, Set.empty[(Int, Int)], startTime, 0L, 0) // Retorna o estado inicial do jogo
  }

  def gameLoop(gameState: GameState): Unit = {
    val currentTime = System.currentTimeMillis() // Tempo atual
    val timeElapsed = (currentTime - gameState.startTime) / 1000 // Tempo decorrido em segundos

    if (gameState.foundWords.size == gameState.allowedWords.size) {
      endGame(gameState)
    } else if (timeElapsed >= 300) { // 300 segundos = 5 minutos
      println("O tempo acabou!")
      endGame(gameState)
    } else {
      SopaDeLetrasUtils.printGameState(gameState)
      SopaDeLetrasUtils.showPrompt()
      processInput(gameState)
    }
  }

  def processInput(gameState: GameState): Unit = {
    //SopaDeLetrasUtils.printGameState(gameState) //Mostra o estado atual do jogo
    //SopaDeLetrasUtils.showPrompt() // Exibe as opções disponíveis para o usuário

    SopaDeLetrasUtils.getUserInput match { // Obtém a entrada do usuário
      case "P" =>
        println("Digite a palavra:")
        val word = readLine.trim.toUpperCase
        println("Digite a coordenada de início (formato: x,y):")
        val Array(x, y) = readLine.trim.split(",").map(_.toInt)
        println("Digite a direção (N, S, E, W, NE, NW, SE, SW):")
        val direction = SopaDeLetrasUtils.getDirectionFromUserInput(readLine.trim.toUpperCase)

        val wordCoords = positions.find { case (w, _) => w == word }
          .map(_._2) // Pega as coordenadas da palavra, se existirem
          .getOrElse(List.empty[(Int, Int)])

        val updatedGameState = playAndUpdate(gameState, word, (x, y), direction.get, wordCoords)
        if (updatedGameState.foundWords.contains(word)) {
          println(s"A palavra '$word' foi encontrada!")
          gameLoop(updatedGameState)
        } else {
          println(s"A palavra '$word' não foi encontrada.")
          gameLoop(gameState)
        }
      case "R" =>
        // Reinicializa um novo estado do jogo com uma semente padrão ou aleatória.
        val (words, positions) = readFile(filePath)
        val newState = initializeGameState(new Random().nextLong(), wordsFromFile, positions)
        gameLoop(newState)
      case "Q" =>
        // Termina o jogo.
        endGame(gameState)
      case _ =>
        println("Opção inválida, tente novamente.")
        gameLoop(gameState)
    }
  }

    def endGame(gameState: GameState): Unit = {
      val endTime = System.currentTimeMillis()
      val totalTime = (endTime - gameState.startTime) / 1000

      // Atualize o tempo de término no estado do jogo
      val finalGameState = gameState.copy(endTime = endTime)

      // Imprima o estado final do tabuleiro e as informações do jogo
      SopaDeLetrasUtils.printGameState(finalGameState)
      
      println(s"Jogo terminado. Sua pontuação foi: ${gameState.score}. Tempo total de jogo: $totalTime segundos.")
      SopaDeLetrasUtils.printGameOver()
      System.exit(0)
    }

  gameLoop(gameState)
}
