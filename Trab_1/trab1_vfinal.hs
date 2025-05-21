-- Grupo: Renan Igor de Lima Ferreira (23104230) / Pedro Henrique Broering Zimmermann (23103753) / André Pinheiro Paes (23205038)

-- Tipos utilizados para facilitar a leitura
type Tabuleiro = [[Int]]                -- Uma matriz 9x9 de inteiros representando o Sudoku
type Coordenadas = (Int, Int)          -- Uma coordenada (linha, coluna)
type Comparacao = ((Coordenadas, Coordenadas), Char)  -- Restrição entre duas coordenadas com um operador

-- Verifica se uma posição está vazia (no nosso caso, usamos -1 para representar uma célula vazia)
posicaoVazia :: Int -> Bool
posicaoVazia n = n == -1

-- Percorre uma lista de coordenadas e retorna a primeira que estiver vazia
acessaElementosMatriz :: Tabuleiro -> [Coordenadas] -> Maybe Coordenadas
acessaElementosMatriz _ [] = Nothing
acessaElementosMatriz tabuleiro ((i, j):xs) =
    if posicaoVazia ((tabuleiro !! i) !! j)
        then Just (i, j)
        else acessaElementosMatriz tabuleiro xs

-- Gera todas as coordenadas da matriz e procura pela primeira posição vazia
verificarPosicaoVazia :: Tabuleiro -> Maybe Coordenadas
verificarPosicaoVazia tabuleiro =
    let indices = [ (i, j) | i <- [0 .. 8], j <- [0 .. 8] ]
    in acessaElementosMatriz tabuleiro indices

-- Verifica se um valor pode ser colocado em uma determinada posição
ehPosicaoValida :: Tabuleiro -> Int -> Int -> Int -> [Comparacao] -> Bool
ehPosicaoValida matriz chute linha coluna comparacoes =
    let linha_valida = notElem chute (matriz !! linha)  -- valor não pode estar na linha
        coluna_valida = notElem chute [ matriz !! i !! coluna | i <- [0..8] ]  -- nem na coluna
        -- Determina a posição inicial do quadrante 3x3
        linha_inicio = (linha `div` 3) * 3
        coluna_inicio = (coluna `div` 3) * 3
        -- Obtém os elementos do quadrante
        quadrante = [ matriz !! l !! c | l <- [linha_inicio..linha_inicio+2], c <- [coluna_inicio..coluna_inicio+2] ]
        quadrante_valido = notElem chute quadrante  -- valor não pode estar no quadrante
        -- Verifica se o valor respeita as comparações adicionais
        comparacoes_validas = all (verificaComparacao matriz chute (linha, coluna)) comparacoes
    in linha_valida && coluna_valida && quadrante_valido && comparacoes_validas

-- Verifica se uma comparação (como A < B) é satisfeita ao inserir o valor em determinada posição
verificaComparacao :: Tabuleiro -> Int -> Coordenadas -> Comparacao -> Bool
verificaComparacao matriz chute pos ((p1, p2), op)
    | pos == p1 =  -- Se estamos tentando preencher p1
        let val2 = matriz !! fst p2 !! snd p2
        in if val2 == -1 then True else compara chute val2 op  -- Se p2 estiver vazio, ainda não dá pra comparar
    | pos == p2 =  -- Se estamos tentando preencher p2
        let val1 = matriz !! fst p1 !! snd p1
        in if val1 == -1 then True else compara val1 chute op  -- Se p1 estiver vazio, ainda não dá pra comparar
    | otherwise = True  -- Se nenhuma das posições da comparação for a atual, ignora

-- Executa a comparação entre dois inteiros com base no operador fornecido
compara :: Int -> Int -> Char -> Bool
compara a b '<' = a < b
compara a b '>' = a > b
compara a b '^' = a < b
compara a b 'v' = a > b
compara _ _ _   = True  

-- Atualiza uma posição da matriz com um novo valor
atualizarMatriz :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
atualizarMatriz tabuleiro linha coluna valor =
    take linha tabuleiro ++
    [take coluna (tabuleiro !! linha) ++ [valor] ++ drop (coluna + 1) (tabuleiro !! linha)] ++
    drop (linha + 1) tabuleiro

-- Tenta inserir todos os valores de 1 a 9 na posição especificada e resolve recursivamente
possiveisChutes :: Tabuleiro -> [Int] -> Int -> Int -> [Comparacao] -> Maybe Tabuleiro
possiveisChutes _ [] _ _ _ = Nothing  -- Se não houver mais opções, falha
possiveisChutes matriz (chute:xs) linha coluna comparacoes
    | ehPosicaoValida matriz chute linha coluna comparacoes =
        let novaMatriz = atualizarMatriz matriz linha coluna chute
        in case resolveSudoku novaMatriz comparacoes of
            Just solucao -> Just solucao  -- Solução encontrada
            Nothing -> possiveisChutes matriz xs linha coluna comparacoes  -- Tenta próximo valor
    | otherwise = possiveisChutes matriz xs linha coluna comparacoes  -- Valor inválido, tenta o próximo

-- Função principal recursiva que resolve o Sudoku
resolveSudoku :: Tabuleiro -> [Comparacao] -> Maybe Tabuleiro
resolveSudoku tabuleiro comparacoes =
    case verificarPosicaoVazia tabuleiro of
        Nothing -> Just tabuleiro  -- Não há mais posições vazias: solução encontrada
        Just (linha, coluna) ->
            let chutes = [1..9]
            in possiveisChutes tabuleiro chutes linha coluna comparacoes  -- Tenta preencher a próxima posição

-- Imprime o tabuleiro linha por linha
imprimirTabuleiro :: Tabuleiro -> IO ()
imprimirTabuleiro = mapM_ print

-- Tabuleiro inicial totalmente vazio (preenchido com -1)
entradaSudoku :: Tabuleiro
entradaSudoku = replicate 9 (replicate 9 (-1))

-- Lista de restrições personalizadas entre posições do tabuleiro
restricoesComparacao :: [Comparacao]
restricoesComparacao = [
    -- ENTRADA 11
    
    -- Linha 0
    (((0, 0), (0, 1)), '<'),
    (((0, 1), (0, 2)), '>'),
    (((0, 3), (0, 4)), '<'),
    (((0, 4), (0, 5)), '<'),
    (((0, 6), (0, 7)), '>'),
    (((0, 7), (0, 8)), '>'),

    -- Linha 1
    (((1, 0), (1, 1)), '<'),
    (((1, 1), (1, 2)), '<'),
    (((1, 3), (1, 4)), '<'),
    (((1, 4), (1, 5)), '>'),
    (((1, 6), (1, 7)), '>'),
    (((1, 7), (1, 8)), '<'),

    -- Linha 2
    (((2, 0), (2, 1)), '<'),
    (((2, 1), (2, 2)), '<'),
    (((2, 3), (2, 4)), '<'),
    (((2, 4), (2, 5)), '>'),
    (((2, 6), (2, 7)), '>'),
    (((2, 7), (2, 8)), '<'),

    -- Linha 3
    (((3, 0), (3, 1)), '>'),
    (((3, 1), (3, 2)), '>'),
    (((3, 3), (3, 4)), '>'),
    (((3, 4), (3, 5)), '<'),
    (((3, 6), (3, 7)), '<'),
    (((3, 7), (3, 8)), '>'),

    -- Linha 4
    (((4, 0), (4, 1)), '>'),
    (((4, 1), (4, 2)), '<'),
    (((4, 3), (4, 4)), '>'),
    (((4, 4), (4, 5)), '<'),
    (((4, 6), (4, 7)), '>'),
    (((4, 7), (4, 8)), '>'),

    -- Linha 5
    (((5, 0), (5, 1)), '<'),
    (((5, 1), (5, 2)), '>'),
    (((5, 3), (5, 4)), '<'),
    (((5, 4), (5, 5)), '>'),
    (((5, 6), (5, 7)), '>'),
    (((5, 7), (5, 8)), '<'),

    -- Linha 6
    (((6, 0), (6, 1)), '<'),
    (((6, 1), (6, 2)), '>'),
    (((6, 3), (6, 4)), '>'),
    (((6, 4), (6, 5)), '>'),
    (((6, 6), (6, 7)), '<'),
    (((6, 7), (6, 8)), '>'),

    -- Linha 7
    (((7, 0), (7, 1)), '>'),
    (((7, 1), (7, 2)), '<'),
    (((7, 3), (7, 4)), '>'),
    (((7, 4), (7, 5)), '<'),
    (((7, 6), (7, 7)), '<'),
    (((7, 7), (7, 8)), '<'),

    -- Linha 8
    (((8, 0), (8, 1)), '<'),
    (((8, 1), (8, 2)), '>'),
    (((8, 3), (8, 4)), '<'),
    (((8, 4), (8, 5)), '<'),
    (((8, 6), (8, 7)), '<'),
    (((8, 7), (8, 8)), '>'),

    -- Coluna 0
    (((0, 0), (1, 0)), 'v'),
    (((1, 0), (2, 0)), '^'),
    (((3, 0), (4, 0)), 'v'),
    (((4, 0), (5, 0)), 'v'),
    (((6, 0), (7, 0)), 'v'),
    (((7, 0), (8, 0)), '^'),

    -- Coluna 1
    (((0, 1), (1, 1)), '^'),
    (((1, 1), (2, 1)), '^'),
    (((3, 1), (4, 1)), '^'),
    (((4, 1), (5, 1)), '^'),
    (((6, 1), (7, 1)), 'v'),
    (((7, 1), (8, 1)), '^'),

    -- Coluna 2
    (((0, 2), (1, 2)), '^'),
    (((1, 2), (2, 2)), '^'),
    (((3, 2), (4, 2)), '^'),
    (((4, 2), (5, 2)), '^'),
    (((6, 2), (7, 2)), 'v'),
    (((7, 2), (8, 2)), 'v'),

    -- Coluna 3
    (((0, 3), (1, 3)), 'v'),
    (((1, 3), (2, 3)), '^'),
    (((3, 3), (4, 3)), 'v'),
    (((4, 3), (5, 3)), '^'),
    (((6, 3), (7, 3)), '^'),
    (((7, 3), (8, 3)), 'v'),

    -- Coluna 4
    (((0, 4), (1, 4)), 'v'),
    (((1, 4), (2, 4)), 'v'),
    (((3, 4), (4, 4)), 'v'),
    (((4, 4), (5, 4)), '^'),
    (((6, 4), (7, 4)), 'v'),
    (((7, 4), (8, 4)), 'v'),

    -- Coluna 5
    (((0, 5), (1, 5)), 'v'),
    (((1, 5), (2, 5)), 'v'),
    (((3, 5), (4, 5)), 'v'),
    (((4, 5), (5, 5)), 'v'),
    (((6, 5), (7, 5)), 'v'),
    (((7, 5), (8, 5)), '^'),

    -- Coluna 6
    (((0, 6), (1, 6)), '^'),
    (((1, 6), (2, 6)), 'v'),
    (((3, 6), (4, 6)), '^'),
    (((4, 6), (5, 6)), 'v'),
    (((6, 6), (7, 6)), '^'),
    (((7, 6), (8, 6)), 'v'),

    -- Coluna 7
    (((0, 7), (1, 7)), 'v'),
    (((1, 7), (2, 7)), 'v'),
    (((3, 7), (4, 7)), 'v'),
    (((4, 7), (5, 7)), 'v'),
    (((6, 7), (7, 7)), '^'),
    (((7, 7), (8, 7)), '^'),

    -- Coluna 8
    (((0, 8), (1, 8)), '^'),
    (((1, 8), (2, 8)), 'v'),
    (((3, 8), (4, 8)), '^'),
    (((4, 8), (5, 8)), 'v'),
    (((6, 8), (7, 8)), '^'),
    (((7, 8), (8, 8)), 'v')

    ]

-- Função principal que executa o programa
main :: IO ()
main = do
    putStrLn "Sudoku Inicial:"
    imprimirTabuleiro entradaSudoku
    putStrLn "\nResolvendo Sudoku..."
    case resolveSudoku entradaSudoku restricoesComparacao of
        Just solucao -> do
            putStrLn "\nSudoku Resolvido:"
            imprimirTabuleiro solucao
        Nothing -> putStrLn "Não foi possível resolver o Sudoku com as restrições fornecidas."
        

{-
    ENTRADA 12

restricoesComparacao :: [Comparacao]
restricoesComparacao = [

    -- Linha 0
    (((0, 0), (0, 1)), '<'),
    (((0, 1), (0, 2)), '>'),
    (((0, 3), (0, 4)), '>'),
    (((0, 4), (0, 5)), '<'),
    (((0, 6), (0, 7)), '<'),
    (((0, 7), (0, 8)), '<'),

    -- Linha 1
    (((1, 0), (1, 1)), '>'),
    (((1, 1), (1, 2)), '<'),
    (((1, 3), (1, 4)), '<'),
    (((1, 4), (1, 5)), '<'),
    (((1, 6), (1, 7)), '>'),
    (((1, 7), (1, 8)), '<'),

    -- Linha 2
    (((2, 0), (2, 1)), '>'),
    (((2, 1), (2, 2)), '<'),
    (((2, 3), (2, 4)), '<'),
    (((2, 4), (2, 5)), '>'),
    (((2, 6), (2, 7)), '<'),
    (((2, 7), (2, 8)), '>'),

    -- Linha 3
    (((3, 0), (3, 1)), '<'),
    (((3, 1), (3, 2)), '<'),
    (((3, 3), (3, 4)), '>'),
    (((3, 4), (3, 5)), '<'),
    (((3, 6), (3, 7)), '>'),
    (((3, 7), (3, 8)), '<'),

    -- Linha 4
    (((4, 0), (4, 1)), '<'),
    (((4, 1), (4, 2)), '>'),
    (((4, 3), (4, 4)), '>'),
    (((4, 4), (4, 5)), '<'),
    (((4, 6), (4, 7)), '>'),
    (((4, 7), (4, 8)), '>'),

    -- Linha 5
    (((5, 0), (5, 1)), '>'),
    (((5, 1), (5, 2)), '>'),
    (((5, 3), (5, 4)), '>'),
    (((5, 4), (5, 5)), '<'),
    (((5, 6), (5, 7)), '<'),
    (((5, 7), (5, 8)), '>'),

    -- Linha 6
    (((6, 0), (6, 1)), '>'),
    (((6, 1), (6, 2)), '<'),
    (((6, 3), (6, 4)), '<'),
    (((6, 4), (6, 5)), '>'),
    (((6, 6), (6, 7)), '<'),
    (((6, 7), (6, 8)), '<'),

    -- Linha 7
    (((7, 0), (7, 1)), '>'),
    (((7, 1), (7, 2)), '<'),
    (((7, 3), (7, 4)), '>'),
    (((7, 4), (7, 5)), '>'),
    (((7, 6), (7, 7)), '>'),
    (((7, 7), (7, 8)), '>'),

    -- Linha 8
    (((8, 0), (8, 1)), '<'),
    (((8, 1), (8, 2)), '>'),
    (((8, 3), (8, 4)), '<'),
    (((8, 4), (8, 5)), '<'),
    (((8, 6), (8, 7)), '<'),
    (((8, 7), (8, 8)), '<'),

    -- Coluna 0
    (((0, 0), (1, 0)), '^'),
    (((1, 0), (2, 0)), '^'),
    (((3, 0), (4, 0)), 'v'),
    (((4, 0), (5, 0)), '^'),
    (((6, 0), (7, 0)), '^'),
    (((7, 0), (8, 0)), 'v'),

    -- Coluna 1
    (((0, 1), (1, 1)), 'v'),
    (((1, 1), (2, 1)), 'v'),
    (((3, 1), (4, 1)), 'v'),
    (((4, 1), (5, 1)), 'v'),
    (((6, 1), (7, 1)), '^'),
    (((7, 1), (8, 1)), '^'),

    -- Coluna 2
    (((0, 2), (1, 2)), '^'),
    (((1, 2), (2, 2)), 'v'),
    (((3, 2), (4, 2)), 'v'),
    (((4, 2), (5, 2)), '^'),
    (((6, 2), (7, 2)), 'v'),
    (((7, 2), (8, 2)), 'v'),

    -- Coluna 3
    (((0, 3), (1, 3)), '^'),
    (((1, 3), (2, 3)), '^'),
    (((3, 3), (4, 3)), '^'),
    (((4, 3), (5, 3)), '^'),
    (((6, 3), (7, 3)), '^'),
    (((7, 3), (8, 3)), 'v'),

    -- Coluna 4
    (((0, 4), (1, 4)), '^'),
    (((1, 4), (2, 4)), 'v'),
    (((3, 4), (4, 4)), '^'),
    (((4, 4), (5, 4)), '^'),
    (((6, 4), (7, 4)), 'v'),
    (((7, 4), (8, 4)), '^'),

    -- Coluna 5
    (((0, 5), (1, 5)), '^'),
    (((1, 5), (2, 5)), 'v'),
    (((3, 5), (4, 5)), '^'),
    (((4, 5), (5, 5)), 'v'),
    (((6, 5), (7, 5)), 'v'),
    (((7, 5), (8, 5)), '^'),

    -- Coluna 6
    (((0, 6), (1, 6)), 'v'),
    (((1, 6), (2, 6)), '^'),
    (((3, 6), (4, 6)), '^'),
    (((4, 6), (5, 6)), 'v'),
    (((6, 6), (7, 6)), '^'),
    (((7, 6), (8, 6)), 'v'),

    -- Coluna 7
    (((0, 7), (1, 7)), 'v'),
    (((1, 7), (2, 7)), '^'),
    (((3, 7), (4, 7)), '^'),
    (((4, 7), (5, 7)), '^'),
    (((6, 7), (7, 7)), 'v'),
    (((7, 7), (8, 7)), '^'),

    -- Coluna 8
    (((0, 8), (1, 8)), 'v'),
    (((1, 8), (2, 8)), 'v'),
    (((3, 8), (4, 8)), 'v'),
    (((4, 8), (5, 8)), 'v'),
    (((6, 8), (7, 8)), 'v'),
    (((7, 8), (8, 8)), '^')
    ]
-}