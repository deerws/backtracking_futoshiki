from pprint import pprint

def find_next_empty(puzzle):
    for r in range(9):
        for c in range(9):
            if puzzle[r][c] == -1:
                return r, c
    return None, None

def is_valid(puzzle, guess, row, col, comparisons):
    # Verifica linha
    if guess in puzzle[row]:
        return False
    # Verifica coluna
    if guess in [puzzle[i][col] for i in range(9)]:
        return False
    # Verifica subgrade 3x3
    row_start = (row // 3) * 3
    col_start = (col // 3) * 3
    for r in range(row_start, row_start + 3):
        for c in range(col_start, col_start + 3):
            if puzzle[r][c] == guess:
                return False
    # Verifica restrições de comparação
    for ((r1, c1), (r2, c2)), op in comparisons.items():
        val1 = guess if (r1, c1) == (row, col) else puzzle[r1][c1]
        val2 = guess if (r2, c2) == (row, col) else puzzle[r2][c2]
        if val1 != -1 and val2 != -1:
            if op == '<' and not val1 < val2:
                return False
            if op == '>' and not val1 > val2:
                return False
            if op == '^' and not val1 < val2:
                return False
            if op == 'v' and not val1 > val2:
                return False
    return True

def solve_sudoku(puzzle, comparisons):
    row, col = find_next_empty(puzzle)
    if row is None:
        return True
    for guess in range(1, 10):
        if is_valid(puzzle, guess, row, col, comparisons):
            puzzle[row][col] = guess
            if solve_sudoku(puzzle, comparisons):
                return True
            puzzle[row][col] = -1
    return False

if __name__ == '__main__':
    example_board = [[-1 for _ in range(9)] for _ in range(9)]
    comparisons = {
         #   Linha 0
        ((0, 0), (0, 1)): '<',
        ((0, 1), (0, 2)): '>',
        ((0, 3), (0, 4)): '<',
        ((0, 4), (0, 5)): '<',
        ((0, 6), (0, 7)): '>',
        ((0, 7), (0, 8)): '>',
    
        #   Linha 1
        ((1, 0), (1, 1)): '<',
        ((1, 1), (1, 2)): '<',
        ((1, 3), (1, 4)): '<',
        ((1, 4), (1, 5)): '>',
        ((1, 6), (1, 7)): '>',
        ((1, 7), (1, 8)): '<',
        
        #   Linha 2
        ((2, 0), (2, 1)): '<',
        ((2, 1), (2, 2)): '<',
        ((2, 3), (2, 4)): '<',
        ((2, 4), (2, 5)): '>',
        ((2, 6), (2, 7)): '>',
        ((2, 7), (2, 8)): '<',
        
        #   Linha 3
        ((3, 0), (3, 1)): '>',
        ((3, 1), (3, 2)): '>',
        ((3, 3), (3, 4)): '>',
        ((3, 4), (3, 5)): '<',
        ((3, 6), (3, 7)): '<',
        ((3, 7), (3, 8)): '>',
        
        #   Linha 4
        ((4, 0), (4, 1)): '>',
        ((4, 1), (4, 2)): '<',
        ((4, 3), (4, 4)): '>',
        ((4, 4), (4, 5)): '<',
        ((4, 6), (4, 7)): '>',
        ((4, 7), (4, 8)): '>',
        
        #   Linha 5
        ((5, 0), (5, 1)): '<',
        ((5, 1), (5, 2)): '>',
        ((5, 3), (5, 4)): '<',
        ((5, 4), (5, 5)): '>',
        ((5, 6), (5, 7)): '>',
        ((5, 7), (5, 8)): '<',
        
        #   Linha 6
        ((6, 0), (6, 1)): '<',
        ((6, 1), (6, 2)): '>',
        ((6, 3), (6, 4)): '>',
        ((6, 4), (6, 5)): '>',
        ((6, 6), (6, 7)): '<',
        ((6, 7), (6, 8)): '>',
        
        #   Linha 7
        ((7, 0), (7, 1)): '>',
        ((7, 1), (7, 2)): '<',
        ((7, 3), (7, 4)): '>',
        ((7, 4), (7, 5)): '<',
        ((7, 6), (7, 7)): '<',
        ((7, 7), (7, 8)): '<',
        
        #   Linha 8
        ((8, 0), (8, 1)): '<',
        ((8, 1), (8, 2)): '>',
        ((8, 3), (8, 4)): '<',
        ((8, 4), (8, 5)): '<',
        ((8, 6), (8, 7)): '<',
        ((8, 7), (8, 8)): '>',
        
        #   Coluna 0
        ((0, 0), (1, 0)): 'v',
        ((1, 0), (2, 0)): '^',
        ((3, 0), (4, 0)): 'v',
        ((4, 0), (5, 0)): 'v',
        ((6, 0), (7, 0)): 'v',
        ((7, 0), (8, 0)): '^',
        
        #   Coluna 1
        ((0, 1), (1, 1)): '^',
        ((1, 1), (2, 1)): '^',
        ((3, 1), (4, 1)): '^',
        ((4, 1), (5, 1)): '^',
        ((6, 1), (7, 1)): 'v',
        ((7, 1), (8, 1)): '^',
        
        #   Coluna 2
        ((0, 2), (1, 2)): '^',
        ((1, 2), (2, 2)): '^',
        ((3, 2), (4, 2)): '^',
        ((4, 2), (5, 2)): '^',
        ((6, 2), (7, 2)): 'v',
        ((7, 2), (8, 2)): 'v',
        
        #   Coluna 3
        ((0, 3), (1, 3)): 'v',
        ((1, 3), (2, 3)): '^',
        ((3, 3), (4, 3)): 'v',
        ((4, 3), (5, 3)): '^',
        ((6, 3), (7, 3)): '^',
        ((7, 3), (8, 3)): 'v',
        
        #   Coluna 4
        ((0, 4), (1, 4)): 'v',
        ((1, 4), (2, 4)): 'v',
        ((3, 4), (4, 4)): 'v',
        ((4, 4), (5, 4)): '^',
        ((6, 4), (7, 4)): 'v',
        ((7, 4), (8, 4)): 'v',
        
        #   Coluna 5
        ((0, 5), (1, 5)): 'v',
        ((1, 5), (2, 5)): 'v',
        ((3, 5), (4, 5)): 'v',
        ((4, 5), (5, 5)): 'v',
        ((6, 5), (7, 5)): 'v',
        ((7, 5), (8, 5)): '^',
        
        #   Coluna 6
        ((0, 6), (1, 6)): '^',
        ((1, 6), (2, 6)): 'v',
        ((3, 6), (4, 6)): '^',
        ((4, 6), (5, 6)): 'v',
        ((6, 6), (7, 6)): '^',
        ((7, 6), (8, 6)): 'v',
        
        #   Coluna 7
        ((0, 7), (1, 7)): 'v',
        ((1, 7), (2, 7)): 'v',
        ((3, 7), (4, 7)): 'v',
        ((4, 7), (5, 7)): 'v',
        ((6, 7), (7, 7)): '^',
        ((7, 7), (8, 7)): '^',
        
        #   Coluna 8
        ((0, 8), (1, 8)): '^',
        ((1, 8), (2, 8)): 'v',
        ((3, 8), (4, 8)): '^',
        ((4, 8), (5, 8)): 'v',
        ((6, 8), (7, 8)): '^',
        ((7, 8), (8, 8)): 'v'
    }
    if solve_sudoku(example_board, comparisons):
        pprint(example_board)
    else:
        print("Não foi possível resolver o Sudoku com as restrições fornecidas.")
