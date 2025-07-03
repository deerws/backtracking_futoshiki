import copy
import numpy as np
import argparse
import sys
import time
from collections import defaultdict

SIZE = 4  # Define o tamanho do tabuleiro

def listify(lst):
    '''
    turns the states into list of lists
    returnlst = list of lists (2D matrix for the puzzle)
    '''
    returnlst = []
    for item in lst:
        nums = [int(x) if x.isdigit() else x for x in item.split()]
        returnlst.append(nums)
    return returnlst

def printlst(lst, f):
    '''
    formatting the list to print out each item in the list
    '''
    for item in lst:
        for enter in item:
            f.write(str(enter))
            f.write(" ")
        f.write('\n')
    f.write('\n')

class BackTracker():
    class Board():
        def __init__(self, puzzle_arr, dom_arr, h_const, v_const, parent=None):
            self.size = SIZE
            self.puzzle = puzzle_arr
            self.domains = dom_arr
            self.h_const = h_const
            self.v_const = v_const
            self.children = []
            self.parent = parent
            self.target = (0, 0)
            self.target_vals = []
            self.target_index = 0
            
            # Cache para otimização
            self._affected_cells = defaultdict(set)
            self._initialize_affected_cells()

        def _initialize_affected_cells(self):
            """Pre-computa células afetadas por cada posição para otimização"""
            for r in range(self.size):
                for c in range(self.size):
                    # Mesma linha e coluna
                    self._affected_cells[(r,c)].update((r,i) for i in range(self.size) if i != c)
                    self._affected_cells[(r,c)].update((i,c) for i in range(self.size) if i != r)
                    
                    # Restrições horizontais
                    if c > 0 and (r,c-1) in self.h_const:
                        self._affected_cells[(r,c)].add((r,c-1))
                    if (r,c) in self.h_const:
                        self._affected_cells[(r,c)].add((r,c+1))
                        
                    # Restrições verticais
                    if r > 0 and (r-1,c) in self.v_const:
                        self._affected_cells[(r,c)].add((r-1,c))
                    if (r,c) in self.v_const:
                        self._affected_cells[(r,c)].add((r+1,c))

        def initialize(self):
            """Inicializa domínios e aplica consistência inicial"""
            # Inicializa domínios
            self.domains = [[{1,2,3,4} if self.puzzle[r][c] == 0 else {self.puzzle[r][c]} 
                           for c in range(self.size)] for r in range(self.size)]
            
            # Aplica consistência inicial para valores pré-preenchidos
            changed = True
            while changed:
                changed = False
                for r in range(self.size):
                    for c in range(self.size):
                        if self.puzzle[r][c] != 0:
                            if not self._propagate_constraints(r, c, self.puzzle[r][c]):
                                return False
                            changed = True
            return True

        def _propagate_constraints(self, row, col, value):
            """Propaga restrições após atribuir um valor"""
            for affected_r, affected_c in self._affected_cells[(row,col)]:
                if value in self.domains[affected_r][affected_c]:
                    # Regra de linha/coluna
                    if row == affected_r or col == affected_c:
                        self.domains[affected_r][affected_c].discard(value)
                        if not self.domains[affected_r][affected_c]:
                            return False
                    
                    # Restrições horizontais
                    if col + 1 == affected_c and (row,col) in self.h_const:
                        ineq = self.h_const[(row,col)]
                        to_remove = {v for v in self.domains[affected_r][affected_c] if 
                                   (ineq == 1 and v >= value) or (ineq == 0 and v <= value)}
                        self.domains[affected_r][affected_c] -= to_remove
                        if not self.domains[affected_r][affected_c]:
                            return False
                            
                    elif col - 1 == affected_c and (affected_r,affected_c) in self.h_const:
                        ineq = self.h_const[(affected_r,affected_c)]
                        to_remove = {v for v in self.domains[affected_r][affected_c] if 
                                   (ineq == 1 and v <= value) or (ineq == 0 and v >= value)}
                        self.domains[affected_r][affected_c] -= to_remove
                        if not self.domains[affected_r][affected_c]:
                            return False
                    
                    # Restrições verticais
                    if row + 1 == affected_r and (row,col) in self.v_const:
                        ineq = self.v_const[(row,col)]
                        to_remove = {v for v in self.domains[affected_r][affected_c] if 
                                   (ineq == 1 and v >= value) or (ineq == 0 and v <= value)}
                        self.domains[affected_r][affected_c] -= to_remove
                        if not self.domains[affected_r][affected_c]:
                            return False
                            
                    elif row - 1 == affected_r and (affected_r,affected_c) in self.v_const:
                        ineq = self.v_const[(affected_r,affected_c)]
                        to_remove = {v for v in self.domains[affected_r][affected_c] if 
                                   (ineq == 1 and v <= value) or (ineq == 0 and v >= value)}
                        self.domains[affected_r][affected_c] -= to_remove
                        if not self.domains[affected_r][affected_c]:
                            return False
            return True

        def update(self):
            """Aplica consistência de arco otimizada"""
            # Mantém uma fila de células para revisar
            queue = [(r,c) for r in range(self.size) for c in range(self.size) 
                    if self.puzzle[r][c] != 0]
            
            while queue:
                r, c = queue.pop(0)
                value = self.puzzle[r][c]
                
                if not self._propagate_constraints(r, c, value):
                    return False
                    
                # Adiciona células afetadas à fila se seus domínios foram reduzidos a um valor
                for affected_r, affected_c in self._affected_cells[(r,c)]:
                    if self.puzzle[affected_r][affected_c] == 0 and len(self.domains[affected_r][affected_c]) == 1:
                        self.puzzle[affected_r][affected_c] = next(iter(self.domains[affected_r][affected_c]))
                        queue.append((affected_r, affected_c))
            
            return True

        def chooseTargetVal(self):
            """Escolhe próxima variável usando MRV + Degree e ordena valores por LCV"""
            # MRV - Minimum Remaining Values
            min_domain_size = float('inf')
            candidates = []
            
            for r in range(self.size):
                for c in range(self.size):
                    if self.puzzle[r][c] == 0:
                        domain_size = len(self.domains[r][c])
                        if domain_size == 0:
                            self.target = (-1,-1)
                            self.target_vals = []
                            return
                        if domain_size < min_domain_size:
                            min_domain_size = domain_size
                            candidates = [(r,c)]
                        elif domain_size == min_domain_size:
                            candidates.append((r,c))

            if not candidates:
                self.target = (-1,-1)
                self.target_vals = []
                return

            # Degree Heuristic - Escolhe variável com mais restrições
            if len(candidates) > 1:
                max_degree = -1
                best_candidate = candidates[0]
                
                for r,c in candidates:
                    degree = len([1 for ar,ac in self._affected_cells[(r,c)]
                                if self.puzzle[ar][ac] == 0])
                    if degree > max_degree:
                        max_degree = degree
                        best_candidate = (r,c)
                        
                self.target = best_candidate
            else:
                self.target = candidates[0]

            # LCV - Least Constraining Value
            r,c = self.target
            values = list(self.domains[r][c])
            
            # Ordena valores por número de conflitos nos vizinhos
            def count_conflicts(val):
                conflicts = 0
                for ar,ac in self._affected_cells[(r,c)]:
                    if self.puzzle[ar][ac] == 0:
                        if val in self.domains[ar][ac]:
                            conflicts += 1
                return conflicts
                
            self.target_vals = sorted(values, key=count_conflicts)
            self.target_index = 0

        def isComplete(self):
            """Verifica se o puzzle está completo"""
            return all(all(cell != 0 for cell in row) for row in self.puzzle)

    def __init__(self, initial_arr, h_const, v_const):
        self.root = self.Board(initial_arr, [], h_const, v_const)

    def solve(self):
        curr = self.root
        if not curr.initialize():
            print("Puzzle inicial inconsistente.")
            return None

        node_visits = 0
        max_node_visits = 1000000
        start_time = time.time()

        while True:
            node_visits += 1
            if node_visits > max_node_visits:
                print(f"Limite de {max_node_visits} visitas atingido.")
                return None

            if node_visits % 1000 == 0:  # Reduzido para feedback mais frequente
                elapsed = time.time() - start_time
                print(f"Visitas: {node_visits}... (Tempo: {elapsed:.2f}s)")

            if curr.isComplete():
                elapsed = time.time() - start_time
                print(f"\nSolução encontrada! Visitas: {node_visits} (Tempo: {elapsed:.2f}s)")
                return curr.puzzle

            curr.chooseTargetVal()
            if curr.target == (-1,-1):
                if not curr.target_vals:  # Sem valores possíveis
                    if curr.parent is None:
                        return None
                    curr = curr.parent
                    curr.target_index += 1
                    continue

            if curr.target_vals and curr.target_index < len(curr.target_vals):
                new_puzzle = copy.deepcopy(curr.puzzle)
                new_domains = copy.deepcopy(curr.domains)

                val_to_try = curr.target_vals[curr.target_index]
                target_r, target_c = curr.target
                new_puzzle[target_r][target_c] = val_to_try
                new_domains[target_r][target_c] = {val_to_try}

                child = self.Board(new_puzzle, new_domains, curr.h_const, curr.v_const, curr)
                
                if child.update():
                    curr.children.append(child)
                    curr = child
                else:
                    curr.target_index += 1
            else:
                if curr.parent is None:
                    elapsed = time.time() - start_time
                    print(f"\nBacktrack até a raiz sem solução. Visitas: {node_visits} (Tempo: {elapsed:.2f}s)")
                    return None

                curr.children = []
                curr = curr.parent
                curr.target_index += 1

def gen_constraints(horiz, vert):
    """Gera dicionários de restrições a partir das strings de entrada"""
    HConstraints = {}
    VConstraints = {}
    constraint_dict = {'^': 1, '>': 1, '<': 0, 'v': 0}

    for r in range(len(horiz)):
        line_constraints = horiz[r]
        for c in range(len(line_constraints)):
            if c < len(line_constraints):
                symbol = line_constraints[c]
                if symbol in constraint_dict:
                    HConstraints[(r, c)] = constraint_dict[symbol]

    for r in range(len(vert)):
        line_constraints = vert[r]
        for c in range(len(line_constraints)):
            if c < len(line_constraints):
                symbol = line_constraints[c]
                if symbol in constraint_dict:
                    VConstraints[(r, c)] = constraint_dict[symbol]

    return [HConstraints, VConstraints]

def main():
    parser = argparse.ArgumentParser(description=f'Resolvedor de Futoshiki {SIZE}x{SIZE}')
    parser.add_argument('--infile', type=argparse.FileType('r'), required=True, help='Arquivo de entrada do puzzle')
    parser.add_argument('--outfile', type=argparse.FileType('w'), default=sys.stdout, help='Arquivo de saída da solução (padrão: stdout)')
    args = parser.parse_args()

    try:
        infile_content = args.infile.read().splitlines()
        args.infile.close()
    except Exception as e:
        print(f"Erro ao ler o arquivo de entrada: {e}")
        sys.exit(1)

    lines_raw = [line for line in infile_content if line.strip()]

    if len(lines_raw) >= 3*SIZE-1:
        print("Usando formato compacto (sem linhas em branco separadoras).")
        puzzle_raw = lines_raw[0:SIZE]
        horiz_raw = lines_raw[SIZE:SIZE*2]
        vert_raw = lines_raw[SIZE*2:SIZE*3-1]
    else:
        print("ERRO: Arquivo com formato inválido ou sem linhas suficientes.")
        sys.exit(1)

    try:
        puzzle_lists = []
        for row in puzzle_raw:
            puzzle_lists.append([int(cell) if cell.isdigit() else 0 for cell in row.split()])
        
        if len(puzzle_lists) != SIZE or any(len(row) != SIZE for row in puzzle_lists):
            raise ValueError(f"Formato incorreto no tabuleiro do puzzle. Deve ser {SIZE}x{SIZE}.")
        
        inpdata = np.array(puzzle_lists, dtype=int)
    except Exception as e:
        print(f"Erro ao processar o tabuleiro do puzzle: {e}")
        print("Dados recebidos para o tabuleiro:")
        for row in puzzle_raw:
            print(row)
        sys.exit(1)

    horiz_str = []
    for row in horiz_raw:
        symbols = row.split()
        if len(symbols) != SIZE-1:
            print(f"AVISO: Linha de restrição horizontal com formato incorreto: {row}")
            symbols = (symbols + [' '] * SIZE)[:SIZE-1]
        horiz_str.append(''.join(symbols))

    vert_str = []
    for row in vert_raw:
        symbols = row.split()
        if len(symbols) != SIZE:
            print(f"AVISO: Linha de restrição vertical com formato incorreto: {row}")
            symbols = (symbols + [' '] * SIZE)[:SIZE]
        vert_str.append(''.join(symbols))

    constraints = gen_constraints(horiz_str, vert_str)
    h_const = constraints[0]
    v_const = constraints[1]

    print("--- Puzzle Inicial ---")
    printlst(inpdata, sys.stdout)
    print("--- Restrições H ---")
    print(h_const)
    print("--- Restrições V ---")
    print(v_const)
    print("----------------------")
    print("Resolvendo...")

    solver = BackTracker(inpdata, h_const, v_const)
    solution = solver.solve()

    print("--- Solução ---")
    if solution is not None:
        printlst(solution, args.outfile)
        print(f"Solução escrita em: {args.outfile.name if args.outfile != sys.stdout else 'Console'}")
    else:
        print("Não foi possível encontrar uma solução.")
        args.outfile.write("Sem solucao\n")

    if args.outfile != sys.stdout:
        args.outfile.close()

if __name__ == "__main__":
    main() 