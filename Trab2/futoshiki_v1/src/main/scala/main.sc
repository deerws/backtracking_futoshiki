object FutoshikiSolver {
  type Board = Vector[Vector[Int]]
  type Position = (Int, Int)
  type Comparison = (Position, Position, String)

  // Encontra a próxima célula vazia (-1)
  def findNextEmpty(board: Board): Option[Position] = {
    for {
      r <- 0 until 9
      c <- 0 until 9
      if board(r)(c) == -1
    } return Some((r, c))
    None
  }

  // Verifica se o palpite é válido
  def isValid(board: Board, guess: Int, row: Int, col: Int, comparisons: Map[(Position, Position), String]): Boolean = {
    // Verifica linha
    if (board(row).contains(guess)) return false

    // Verifica coluna
    if (board.map(_(col)).contains(guess)) return false

    // Verifica subgrade 3x3
    val rowStart = (row / 3) * 3
    val colStart = (col / 3) * 3
    val subgrid = for {
      r <- rowStart until (rowStart + 3)
      c <- colStart until (colStart + 3)
    } yield board(r)(c)
    if (subgrid.contains(guess)) return false

    // Verifica restrições de comparação
    comparisons.forall { case ((pos1 @ (r1, c1), pos2 @ (r2, c2)), op) =>
      val val1 = if (pos1 == ((row, col))) guess else board(r1)(c1)
      val val2 = if (pos2 == ((row, col))) guess else board(r2)(c2)
      if (val1 != -1 && val2 != -1) {
        op match {
          case "<" | "^" => val1 < val2
          case ">" | "v" => val1 > val2
          case _ => true
        }
      } else true
    }
  }

  // Resolve o Futoshiki usando backtracking
  def solveSudoku(board: Board, comparisons: Map[(Position, Position), String]): Option[Board] = {
    findNextEmpty(board) match {
      case None => Some(board) // Tabuleiro completo
      case Some((row, col)) =>
        (1 to 9).to(LazyList).flatMap { guess =>
          if (isValid(board, guess, row, col, comparisons)) {
            // Atualiza o tabuleiro imutavelmente
            val newBoard = board.updated(row, board(row).updated(col, guess))
            solveSudoku(newBoard, comparisons)
          } else None
        }.headOption
    }
  }

  // Imprime o tabuleiro
  def printBoard(board: Board): Unit = {
    board.foreach { row =>
      println(row.map {
        case -1 => "."
        case n => n.toString
      }.mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    // Tabuleiro inicial 9x9 com -1 para células vazias
    val exampleBoard: Board = Vector.fill(9, 9)(-1)

    // Restrições de comparação (mesmas do código Python)
    val comparisons: Map[(Position, Position), String] = Map(
      // Linha 0
      ((0, 0), (0, 1)) -> "<", ((0, 1), (0, 2)) -> ">", ((0, 3), (0, 4)) -> "<",
      ((0, 4), (0, 5)) -> "<", ((0, 6), (0, 7)) -> ">", ((0, 7), (0, 8)) -> ">",
      // Linha 1
      ((1, 0), (1, 1)) -> "<", ((1, 1), (1, 2)) -> "<", ((1, 3), (1, 4)) -> "<",
      ((1, 4), (1, 5)) -> ">", ((1, 6), (1, 7)) -> ">", ((1, 7), (1, 8)) -> "<",
      // Linha 2
      ((2, 0), (2, 1)) -> "<", ((2, 1), (2, 2)) -> "<", ((2, 3), (2, 4)) -> "<",
      ((2, 4), (2, 5)) -> ">", ((2, 6), (2, 7)) -> ">", ((2, 7), (2, 8)) -> "<",
      // Linha 3
      ((3, 0), (3, 1)) -> ">", ((3, 1), (3, 2)) -> ">", ((3, 3), (3, 4)) -> ">",
      ((3, 4), (3, 5)) -> "<", ((3, 6), (3, 7)) -> "<", ((3, 7), (3, 8)) -> ">",
      // Linha 4
      ((4, 0), (4, 1)) -> ">", ((4, 1), (4, 2)) -> "<", ((4, 3), (4, 4)) -> ">",
      ((4, 4), (4, 5)) -> "<", ((4, 6), (4, 7)) -> ">", ((4, 7), (4, 8)) -> ">",
      // Linha 5
      ((5, 0), (5, 1)) -> "<", ((5, 1), (5, 2)) -> ">", ((5, 3), (5, 4)) -> "<",
      ((5, 4), (5, 5)) -> ">", ((5, 6), (5, 7)) -> ">", ((5, 7), (5, 8)) -> "<",
      // Linha 6
      ((6, 0), (6, 1)) -> "<", ((6, 1), (6, 2)) -> ">", ((6, 3), (6, 4)) -> ">",
      ((6, 4), (6, 5)) -> ">", ((6, 6), (6, 7)) -> "<", ((6, 7), (6, 8)) -> ">",
      // Linha 7
      ((7, 0), (7, 1)) -> ">", ((7, 1), (7, 2)) -> "<", ((7, 3), (7, 4)) -> ">",
      ((7, 4), (7, 5)) -> "<", ((7, 6), (7, 7)) -> "<", ((7, 7), (7, 8)) -> "<",
      // Linha 8
      ((8, 0), (8, 1)) -> "<", ((8, 1), (8, 2)) -> ">", ((8, 3), (8, 4)) -> "<",
      ((8, 4), (8, 5)) -> "<", ((8, 6), (8, 7)) -> "<", ((8, 7), (8, 8)) -> ">",
      // Coluna 0
      ((0, 0), (1, 0)) -> "v", ((1, 0), (2, 0)) -> "^", ((3, 0), (4, 0)) -> "v",
      ((4, 0), (5, 0)) -> "v", ((6, 0), (7, 0)) -> "v", ((7, 0), (8, 0)) -> "^",
      // Coluna 1
      ((0, 1), (1, 1)) -> "^", ((1, 1), (2, 1)) -> "^", ((3, 1), (4, 1)) -> "^",
      ((4, 1), (5, 1)) -> "^", ((6, 1), (7, 1)) -> "v", ((7, 1), (8, 1)) -> "^",
      // Coluna 2
      ((0, 2), (1, 2)) -> "^", ((1, 2), (2, 2)) -> "^", ((3, 2), (4, 2)) -> "^",
      ((4, 2), (5, 2)) -> "^", ((6, 2), (7, 2)) -> "v", ((7, 2), (8, 2)) -> "v",
      // Coluna 3
      ((0, 3), (1, 3)) -> "v", ((1, 3), (2, 3)) -> "^", ((3, 3), (4, 3)) -> "v",
      ((4, 3), (5, 3)) -> "^", ((6, 3), (7, 3)) -> "^", ((7, 3), (8, 3)) -> "v",
      // Coluna 4
      ((0, 4), (1, 4)) -> "v", ((1, 4), (2, 4)) -> "v", ((3, 4), (4, 4)) -> "v",
      ((4, 4), (5, 4)) -> "^", ((6, 4), (7, 4)) -> "v", ((7, 4), (8, 4)) -> "v",
      // Coluna 5
      ((0, 5), (1, 5)) -> "v", ((1, 5), (2, 5)) -> "v", ((3, 5), (4, 5)) -> "v",
      ((4, 5), (5, 5)) -> "v", ((6, 5), (7, 5)) -> "v", ((7, 5), (8, 5)) -> "^",
      // Coluna 6
      ((0, 6), (1, 6)) -> "^", ((1, 6), (2, 6)) -> "v", ((3, 6), (4, 6)) -> "^",
      ((4, 6), (5, 6)) -> "v", ((6, 6), (7, 6)) -> "^", ((7, 6), (8, 6)) -> "v",
      // Coluna 7
      ((0, 7), (1, 7)) -> "v", ((1, 7), (2, 7)) -> "v", ((3, 7), (4, 7)) -> "v",
      ((4, 7), (5, 7)) -> "v", ((6, 7), (7, 7)) -> "^", ((7, 7), (8, 7)) -> "^",
      // Coluna 8
      ((0, 8), (1, 8)) -> "^", ((1, 8), (2, 8)) -> "v", ((3, 8), (4, 8)) -> "^",
      ((4, 8), (5, 8)) -> "v", ((6, 8), (7, 8)) -> "^", ((7, 8), (8, 8)) -> "v"
    )

    solveSudoku(exampleBoard, comparisons) match {
      case Some(solvedBoard) =>
        println("Tabuleiro resolvido:")
        printBoard(solvedBoard)
      case None =>
        println("Não foi possível resolver o Sudoku com as restrições fornecidas.")
    }
  }
}