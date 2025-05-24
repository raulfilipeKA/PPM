package GrupoLM29_RaulAzevedo_TiagoNunes_FranciscoRibeiro

object Main {
  def main(args: Array[String]): Unit = {
    val tarefas = new Tarefas()
    val rand = MyRandom(42) // alterar a seed para verificar outros resultados
    val size = 3
    val board = tarefas.drawBoard(size)

    println("Tabuleiro inicial:")
    tarefas.printBoard(board)

    // Gerar coordenadas livres inicialmente
    val lstOpenCoords: List[(Int, Int)] = (for {
      i <- 0 until size
      j <- 0 until size
    } yield (i, j)).toList

    // Teste do cellIsEmpty e getValueAt
    val testCoord = (1, 1)
    println(s"Valor na posição $testCoord: ${tarefas.getValueAt(board, testCoord)}")
    println(s"A célula $testCoord está vazia? ${tarefas.cellIsEmpty(board, testCoord)}")

    // Teste do updateBoard
    val updatedBoard = tarefas.updateBoard(board, testCoord, 'B')
    println(s"Tabuleiro depois de updateBoard($testCoord, 'B'):")
    tarefas.printBoard(updatedBoard)

//    // Teste do placeStone
//    val (boardAfterPlacement, updatedCoords) = tarefas.placeStone(board, testCoord, lstOpenCoords)
//    println("Tabuleiro depois de colocar uma pedra:")
//    tarefas.printBoard(boardAfterPlacement)

//    // Teste do removeItem
//    println(s"Coordenadas antes de remover $testCoord: $lstOpenCoords")
//    println(s"Coordenadas depois: ${tarefas.removeItem(lstOpenCoords, testCoord)}")

    // Teste do play
    println("Teste da função play (mesma posição):")
    val (maybeNewBoard, _) = tarefas.play(board, ((0, 0), 'B'), testCoord, lstOpenCoords)
    maybeNewBoard.foreach { b =>
      tarefas.printBoard(b)
    }

    // Teste do playRandomly
    println("Teste do playRandomly:")
    val (newBoard, newRand, newOpenCoords) = tarefas.playRandomly(board, rand, ((0, 0), 'B'), lstOpenCoords, tarefas.randomMove)
    tarefas.printBoard(newBoard)

    println(s"Coordenadas livres restantes: $newOpenCoords")
  }
}
