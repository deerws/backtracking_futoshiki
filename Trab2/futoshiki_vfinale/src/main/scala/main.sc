object sudokuComparacao {
  type Tabuleiro = Vector[Vector[Int]]
  type Coordenadas = (Int, Int)
  type Comparacao = (Coordenadas, Coordenadas, String)

  // Encontra a próxima célula vazia (-1)
  def encontrarCelulaVazia(tabuleiro: Tabuleiro): Option[Coordenadas] = {
    for {
      r <- 0 until 9    // Itera por todas as linhas
      c <- 0 until 9    // Itera por todas as Colunas
      if tabuleiro(r)(c) == -1  // Se a celula esta vazia
    } return Some((r, c))   // Retorna a posicao 
    None    // Retorna nada se não houver celulas vazias
  }

  // Verifica se o palpite é válido
  def ehValido(tabuleiro: Tabuleiro, chute: Int, linha: Int, coluna: Int, Comparacao: Map[(Coordenadas, Coordenadas), String]): Boolean = {
      
    // Verifica se o numero ja existe na linha
    if (tabuleiro(linha).contains(chute)) return false
    
    // Verifica se o numero ja existe na Coluna
    if (tabuleiro.map(_(coluna)).contains(chute)) return false

    // Verifica quadrante 3x3
    val inicioLinha = (linha / 3) * 3
    val inicioColuna = (coluna / 3) * 3
    
    //  Itera sob a quadrante
    val quadrante = for {
      r <- inicioLinha until (inicioLinha + 3)
      c <- inicioColuna until (inicioColuna + 3)
    } yield tabuleiro(r)(c)
    
    // Verifica se o numero ja existe na quadrante
    if (quadrante.contains(chute)) return false

    // Verifica restrições de comparação
    Comparacao.forall { case ((pos1 @ (r1, c1), pos2 @ (r2, c2)), op) =>
    // Se o valor atual for um chute ou o valor do tabuleiro
      val val1 = if (pos1 == ((linha, coluna))) chute else tabuleiro(r1)(c1)  
      val val2 = if (pos2 == ((linha, coluna))) chute else tabuleiro(r2)(c2)
      // verifica se ambos valores estao preenchidos
      if (val1 != -1 && val2 != -1) {
        op match {
          case "<" | "^" => val1 < val2
          case ">" | "v" => val1 > val2
        }
      } else true // Se os valores estiverem vazios retorna true
    }
  }

  // Resolve usando backtracking
  def resolveSudokuComparacao(tabuleiro: Tabuleiro, Comparacao: Map[(Coordenadas, Coordenadas), String]): Option[Tabuleiro] = {
    encontrarCelulaVazia(tabuleiro) match {
      case None => Some(tabuleiro) // Nenhuma celula vazia, ou seja, encontramos a solucao
      case Some((linha, coluna)) =>  // Encontrou celula vazia
        (1 to 9).to(LazyList).flatMap { chute =>    // Testa os numeros de 1-9 e para quando o validar
          if (ehValido(tabuleiro, chute, linha, coluna, Comparacao)) {
              
            // Atualiza o tabuleiro com o chute
            val novotabuleiro = tabuleiro.updated(linha, tabuleiro(linha).updated(coluna, chute))
            resolveSudokuComparacao(novotabuleiro, Comparacao)  // Chama recursivamente
            
          } else None   // Nao encontrou a solucao
        }.headOption    // Retorna a solução encontrada ou None caso não encontre
    }
  }

  // Imprime o tabuleiro
  def printtabuleiro(tabuleiro: Tabuleiro): Unit = {
    tabuleiro.foreach { linha =>
      println(linha.map {
        case -1 => "."
        case n => n.toString
      }.mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    // tabuleiro inicial 9x9 com -1 para células vazias
    val tabuleiroInicial: Tabuleiro = Vector.fill(9, 9)(-1)

    // Restrições de comparação 
    val Comparacao: Map[(Coordenadas, Coordenadas), String] = Map(
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

    resolveSudokuComparacao(tabuleiroInicial, Comparacao) match {
      case Some(solvedTabuleiro) =>
        println("tabuleiro resolvido:")
        printtabuleiro(solvedTabuleiro)
      case None =>
        println("Nao foi possivel resolver o Sudoku com as restricoes fornecidas.")
    }
  }
}