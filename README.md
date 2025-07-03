# Futoshiki Solver

## Description
This repository contains implementations of a Futoshiki solver, a logic puzzle similar to Sudoku but with additional inequality constraints between adjacent cells. The goal is to fill a 9x9 grid with numbers from 1 to 9, adhering to Sudoku rules (each number appears exactly once per row, column, and 3x3 subgrid) and satisfying comparison constraints (>, <, ^, v) between pairs of adjacent cells.

The implementations were developed in four different programming languages:
- **Python**: Using backtracking to solve the puzzle.
- **Haskell**: Functional implementation with custom types for the board and constraints.
- **Scala**: Functional approach with a vector-based board representation.
- **Prolog**: Logic-based implementation leveraging constraint logic programming (CLP).

## Features
- Solves a 9x9 Futoshiki puzzle with given inequality constraints.
- Ensures standard Sudoku rules are followed (unique numbers in rows, columns, and 3x3 subgrids).
- Handles comparison constraints (>, <, ^, v) between adjacent cells.
- Each implementation includes a sample puzzle configuration for testing.

## Project Structure
- `python/futoshiki.py`: Python implementation using a backtracking algorithm.
- `haskell/futoshiki.hs`: Haskell implementation with a functional approach.
- `scala/Futoshiki.scala`: Scala implementation with a vector-based board.
- `prolog/futoshiki.pl`: Prolog implementation using constraint logic programming.

## Requirements
- **Python**: Python 3.x
- **Haskell**: GHC (Glasgow Haskell Compiler)
- **Scala**: Scala 3.x, SBT or another Scala build tool
- **Prolog**: SWI-Prolog

## Usage
1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/futoshiki-solver.git
   cd futoshiki-solver
   ```

2. Run the solver in your preferred language:
   - **Python**:
     ```bash
     python python/futoshiki.py
     ```
   - **Haskell**:
     ```bash
     ghc -o futoshiki haskell/futoshiki.hs
     ./futoshiki
     ```
   - **Scala**:
     ```bash
     sbt "runMain sudokuComparacao"
     ```
   - **Prolog**:
     ```bash
     swipl -s prolog/futoshiki.pl -g solve_example
     ```

3. The solver will output the initial empty board and the solved Futoshiki puzzle if a solution exists, or a message indicating that no solution was found.

## Example
The sample puzzle included in each implementation uses a 9x9 grid with predefined inequality constraints (e.g., `<`, `>`, `^`, `v`) between adjacent cells. The solver fills the grid while ensuring all constraints are satisfied.

Example output (Python):
```python
[[3, 6, 2, 8, 9, 7, 1, 4, 5],
 [7, 8, 9, 5, 6, 4, 3, 2, 1],
 [4, 5, 1, 3, 2, 9, 6, 7, 8],
 [9, 4, 8, 6, 7, 5, 2, 1, 3],
 [6, 3, 7, 1, 4, 2, 9, 8, 5],
 [2, 1, 5, 9, 8, 3, 7, 6, 4],
 [5, 7, 4, 2, 1, 6, 8, 9, 3],
 [1, 9, 3, 4, 5, 8, 6, 7, 2],
 [8, 2, 6, 7, 3, 1, 5, 4, 9]]
```

## Constraints
- The board is represented as a 9x9 grid, with `-1` indicating empty cells.
- Comparison constraints are defined as pairs of coordinates with an operator (`<`, `>`, `^`, `v`).
- The solver ensures that each row, column, and 3x3 subgrid contains unique numbers from 1 to 9.
- Inequality constraints are enforced between adjacent cells as specified.

## Contributors
- André Pinheiro Paes
- Renan Igor de Lima Ferreira
- Pedro Henrique Broering Zimmermann

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
