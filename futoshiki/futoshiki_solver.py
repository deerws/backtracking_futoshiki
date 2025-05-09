import copy
import argparse
import sys
import time
from collections import defaultdict

SIZE = 4  # Define o tamanho do tabuleiro
QUAD_SIZE = 2  # Define o tamanho do quadrante (2x2)

def printlst(lst, f):
    """Formata e imprime a matriz no arquivo/stdout"""
    for row in lst:
        f.write(" ".join(map(str, row)) + "\n")
    f.write('\n')

class BackTracker():
    class Board():
        def __init__(self, puzzle_list, h_const, v_const, parent=None):
            self.size = SIZE
            self.quad_size = QUAD_SIZE
            self.puzzle = puzzle_list
            self.h_const = h_const
            self.v_const = v_const
            self.parent = parent
            self.children = []  # Manter rastreio pode ser útil para debug, mas não essencial para o solve

            self.domains = None  # Será inicializado depois
            self.target = None  # Próxima célula a preencher
            self.target_vals = []  # Valores a tentar para self.target
            self.target_index = 0  # Índice do valor atual em self.target_vals

            # Cache das células afetadas por uma atribuição (otimização)
            self._affected_cells_cache = self._compute_affected_cells()

        def _compute_affected_cells(self):
            """
            Pré-computa e armazena o conjunto de células afetadas (linha, coluna, quadrante)
            por cada célula (r, c). Inclui as células de restrição de desigualdade.
            Retorna um dicionário: {(r, c): set_of_affected_cells}
            """
            affected = defaultdict(set)
            for r in range(self.size):
                for c in range(self.size):
                    coords = (r, c)

                    # 1. Linha (excluindo a própria célula)
                    affected[coords].update((r, col) for col in range(self.size) if col != c)

                    # 2. Coluna (excluindo a própria célula)
                    affected[coords].update((row, c) for row in range(self.size) if row != r)

                    # 3. Quadrante 2x2 (excluindo a própria célula)
                    start_row = (r // self.quad_size) * self.quad_size
                    start_col = (c // self.quad_size) * self.quad_size
                    for qr in range(start_row, start_row + self.quad_size):
                        for qc in range(start_col, start_col + self.quad_size):
                            if (qr, qc) != coords:
                                affected[coords].add((qr, qc))

                    # 4. Desigualdade Horizontal (célula adjacente)
                    if c > 0 and (r, c - 1) in self.h_const:  # Restrição à esquerda
                        affected[coords].add((r, c - 1))
                    if c < self.size - 1 and (r, c) in self.h_const:  # Restrição à direita
                        affected[coords].add((r, c + 1))

                    # 5. Desigualdade Vertical (célula adjacente)
                    if r > 0 and (r - 1, c) in self.v_const:  # Restrição acima
                        affected[coords].add((r - 1, c))
                    if r < self.size - 1 and (r, c) in self.v_const:  # Restrição abaixo
                        affected[coords].add((r + 1, c))

            return affected

        def initialize_domains(self):
            """Inicializa os domínios de todas as células"""
            self.domains = [[set(range(1, self.size + 1)) if self.puzzle[r][c] == 0 else {self.puzzle[r][c]}
                             for c in range(self.size)] for r in range(self.size)]
            return True

        def apply_initial_consistency(self):
            """Aplica consistência inicial baseada nas células pré-preenchidas"""
            for r in range(self.size):
                for c in range(self.size):
                    if self.puzzle[r][c] != 0:
                        # Se a célula já tem valor, seu domínio deve ser apenas ele
                        value = self.puzzle[r][c]
                        self.domains[r][c] = {value}
                        # Propaga a restrição deste valor para os vizinhos
                        if not self._propagate_constraints(r, c, value):
                            return False  # Inconsistência inicial
            return True

        def _propagate_constraints(self, r, c, assigned_value):
            """
            Propaga as restrições DEPOIS que 'assigned_value' foi colocado em (r, c).
            Remove 'assigned_value' dos domínios dos vizinhos na mesma linha, coluna e quadrante.
            Também aplica restrições de desigualdade se o vizinho ainda não foi atribuído.
            Retorna True se consistente, False se algum domínio ficou vazio.
            """
            # 1. Propagação de Unicidade (Linha, Coluna, Quadrante)
            for ar, ac in self._affected_cells_cache[(r, c)]:
                # Só propaga se o vizinho ainda não tem valor E se o valor atribuido está no domínio do vizinho
                if self.puzzle[ar][ac] == 0 and assigned_value in self.domains[ar][ac]:
                    # Verifica se estão na mesma linha, coluna ou quadrante
                    in_same_row = (r == ar)
                    in_same_col = (c == ac)
                    in_same_quad = (r // self.quad_size == ar // self.quad_size) and \
                                   (c // self.quad_size == ac // self.quad_size)

                    if in_same_row or in_same_col or in_same_quad:
                        self.domains[ar][ac].discard(assigned_value)
                        if not self.domains[ar][ac]:
                            return False  # Domínio vazio -> Inconsistência

            # 2. Propagação de Desigualdades (para vizinhos *não preenchidos*)
            # Horizontal - Vizinho à Direita (r, c+1)
            if c < self.size - 1 and (r, c) in self.h_const:
                ar, ac = r, c + 1
                if self.puzzle[ar][ac] == 0:  # Se vizinho não preenchido
                    ineq = self.h_const[(r, c)]  # Restrição entre (r,c) e (r,c+1)
                    vals_to_remove = set()
                    if ineq == 1:  # (r,c) > (r,c+1)  => assigned_value > vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    else:  # (r,c) < (r,c+1)  => assigned_value < vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            # Horizontal - Vizinho à Esquerda (r, c-1)
            if c > 0 and (r, c - 1) in self.h_const:
                ar, ac = r, c - 1
                if self.puzzle[ar][ac] == 0:
                    ineq = self.h_const[(r, c - 1)]  # Restrição entre (r,c-1) e (r,c)
                    vals_to_remove = set()
                    if ineq == 1:  # (r,c-1) > (r,c)  => vizinho > assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    else:  # (r,c-1) < (r,c)  => vizinho < assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            # Vertical - Vizinho Abaixo (r+1, c)
            if r < self.size - 1 and (r, c) in self.v_const:
                ar, ac = r + 1, c
                if self.puzzle[ar][ac] == 0:
                    ineq = self.v_const[(r, c)]  # Restrição entre (r,c) e (r+1,c)
                    vals_to_remove = set()
                    if ineq == 1:  # (r,c) > (r+1,c) (^ na interface) => assigned_value > vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    else:  # (r,c) < (r+1,c) (v na interface) => assigned_value < vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            # Vertical - Vizinho Acima (r-1, c)
            if r > 0 and (r - 1, c) in self.v_const:
                ar, ac = r - 1, c
                if self.puzzle[ar][ac] == 0:
                    ineq = self.v_const[(r - 1, c)]  # Restrição entre (r-1,c) e (r,c)
                    vals_to_remove = set()
                    if ineq == 1:  # (r-1,c) > (r,c) (^ na interface) => vizinho > assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    else:  # (r-1,c) < (r,c) (v na interface) => vizinho < assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            return True  # Estado consistente após propagação

        def choose_next_variable(self):
            """Escolhe próxima variável usando MRV + Degree"""
            min_domain_size = float('inf')
            candidates = []

            for r in range(self.size):
                for c in range(self.size):
                    if self.puzzle[r][c] == 0:  # Apenas células não atribuídas
                        domain_size = len(self.domains[r][c])
                        if domain_size == 0:  # Inconsistência encontrada
                            self.target = (-1, -1)  # Sinaliza erro/beco sem saída
                            return
                        if domain_size < min_domain_size:
                            min_domain_size = domain_size
                            candidates = [(r, c)]
                        elif domain_size == min_domain_size:
                            candidates.append((r, c))

            if not candidates:  # Não há mais células vazias
                self.target = None  # Sinaliza que o puzzle está completo
                return

            # Degree Heuristic (desempate) - Conta vizinhos *não atribuídos* afetados
            if len(candidates) > 1:
                max_degree = -1
                best_candidate = candidates[0]
                for r, c in candidates:
                    # Grau = número de vizinhos não atribuídos no grafo de restrições
                    degree = sum(1 for ar, ac in self._affected_cells_cache[(r, c)]
                                 if self.puzzle[ar][ac] == 0)
                    if degree > max_degree:
                        max_degree = degree
                        best_candidate = (r, c)
                self.target = best_candidate
            else:
                self.target = candidates[0]  # Único candidato MRV

        def get_ordered_values(self):
            """Ordena valores do domínio do target usando LCV"""
            if self.target is None or self.target == (-1, -1):
                self.target_vals = []
                return

            r, c = self.target
            values = list(self.domains[r][c])

            # LCV - Conta quantos valores seriam removidos dos domínios dos vizinhos
            def count_conflicts(val_to_try):
                conflicts = 0
                # Simula o impacto nos vizinhos (sem deepcopy, apenas contando)
                for ar, ac in self._affected_cells_cache[(r, c)]:
                    if self.puzzle[ar][ac] == 0:  # Se vizinho não atribuído
                        # Verifica unicidade linha/coluna/quadrante
                        in_same_row = (r == ar)
                        in_same_col = (c == ac)
                        in_same_quad = (r // self.quad_size == ar // self.quad_size) and \
                                      (c // self.quad_size == ac // self.quad_size)
                        if (in_same_row or in_same_col or in_same_quad) and val_to_try in self.domains[ar][ac]:
                            conflicts += 1  # Contaria como 1 remoção potencial

                        # Verifica desigualdades
                        # Horizontal
                        elif c + 1 == ac and (r, c) in self.h_const and val_to_try in self.domains[ar][ac]: 
                            conflicts += 1
                        elif c - 1 == ac and (ar, ac) in self.h_const and val_to_try in self.domains[ar][ac]: 
                            conflicts += 1
                        # Vertical
                        elif r + 1 == ar and (r, c) in self.v_const and val_to_try in self.domains[ar][ac]: 
                            conflicts += 1
                        elif r - 1 == ar and (ar, ac) in self.v_const and val_to_try in self.domains[ar][ac]: 
                            conflicts += 1

                return conflicts

            # Ordena: menos conflitos primeiro
            self.target_vals = sorted(values, key=count_conflicts)
            self.target_index = 0

        def is_complete(self):
            """Verifica se o puzzle está completo (sem zeros)"""
            return all(self.puzzle[r][c] != 0 for r in range(self.size) for c in range(self.size))

    # --- Fim da classe Board ---

    def __init__(self, initial_puzzle_list, h_const, v_const):
        """Inicializa o resolvedor com o puzzle e restrições"""
        # Cria o estado inicial (raiz da árvore de busca)
        self.root = self.Board(initial_puzzle_list, h_const, v_const)

    def solve(self):
        """Executa o algoritmo de backtracking com propagação de restrições"""
        
        # Inicializa domínios e aplica consistência inicial
        if not self.root.initialize_domains():
            print("Erro na inicialização dos domínios.")
            return None
        if not self.root.apply_initial_consistency():
            print("Puzzle inicial inconsistente.")
            return None

        curr = self.root  # Começa na raiz
        node_visits = 0
        max_node_visits = 2000000  # Limite para evitar loops infinitos
        start_time = time.time()

        while True:
            node_visits += 1
            if node_visits > max_node_visits:
                print(f"Limite de {max_node_visits} visitas atingido.")
                return None

            # Feedback de progresso
            if node_visits % 50000 == 0:
                elapsed = time.time() - start_time
                print(f"Visitas: {node_visits}... (Tempo: {elapsed:.2f}s)")

            # 1. Verifica se o estado atual é uma solução completa
            if curr.is_complete():
                elapsed = time.time() - start_time
                print(f"\nSolução encontrada! Visitas: {node_visits} (Tempo: {elapsed:.2f}s)")
                return curr.puzzle  # Retorna a solução

            # 2. Escolhe a próxima variável (célula) e ordena seus valores
            if curr.target is None:  # Só escolhe se ainda não foi escolhido para este nó
                curr.choose_next_variable()
                curr.get_ordered_values()

            # 3. Verifica se há um caminho a seguir (variável escolhida e valores restantes)
            if curr.target != (-1, -1) and curr.target_vals and curr.target_index < len(curr.target_vals):
                # --- Tentativa (Descer na árvore) ---
                
                # Pega o próximo valor a tentar para a variável escolhida
                val_to_try = curr.target_vals[curr.target_index]
                target_r, target_c = curr.target

                # Cria um *novo* estado filho (Board) fazendo cópias profundas
                new_puzzle = copy.deepcopy(curr.puzzle)
                new_domains = copy.deepcopy(curr.domains)

                # Atribui o valor no novo estado
                new_puzzle[target_r][target_c] = val_to_try
                new_domains[target_r][target_c] = {val_to_try}  # Fixa o domínio

                # Cria o nó filho
                child = self.Board(new_puzzle, self.root.h_const, self.root.v_const, curr)
                child.domains = new_domains  # Atribui os domínios copiados e modificados
                child.target = None  # Filho precisará escolher sua própria variável

                # Propaga as restrições a partir da nova atribuição no filho
                if child._propagate_constraints(target_r, target_c, val_to_try):
                    # ...avança para o estado filho
                    curr = child
                else:
                    # ...se a propagação falhou (inconsistência),
                    # descarta este valor e tenta o próximo no nó atual.
                    curr.target_index += 1

            else:
                # --- Backtrack (Subir na árvore) ---
                if curr.parent is None:
                    # Chegou de volta à raiz e não há mais opções -> Sem solução
                    elapsed = time.time() - start_time
                    print(f"\nBacktrack até a raiz sem solução. Visitas: {node_visits} (Tempo: {elapsed:.2f}s)")
                    return None

                # Sobe para o pai
                curr = curr.parent
                # Prepara para tentar o próximo valor do pai na próxima iteração
                curr.target_index += 1

# --- Fim da classe BackTracker ---

def parse_input_file(filename):
    """
    Processa o arquivo de entrada no novo formato e retorna o puzzle e restrições
    Lida com o formato que usa 'x' para representar ausência de comparação.
    """
    try:
        with open(filename, 'r') as f:
            lines = [line.strip() for line in f if line.strip()]
        
        # Validação básica do número de linhas
        if len(lines) < SIZE * 3 - 1:  # Puzzle + H_const + V_const (última linha V pode ser omitida)
            raise ValueError(f"Formato de arquivo inválido. Esperadas pelo menos {SIZE*3-1} linhas não vazias.")
        
        # Extrai as linhas do puzzle, restrições horizontais e verticais
        puzzle_lines = lines[0:SIZE*2-1:2]  # Linhas 0, 2, 4, 6 (puzzle)
        horiz_lines = lines[1:SIZE*2-1:2]   # Linhas 1, 3, 5, 7 (restrições horizontais)
        vert_lines = lines[SIZE*2-1:]       # Restante das linhas (restrições verticais)
        
        # Parse do puzzle
        puzzle = []
        for line in puzzle_lines:
            row = []
            items = line.split()
            for item in items:
                if item.isdigit():
                    row.append(int(item))
                else:
                    row.append(0)  # Valor 0 representa célula vazia
            puzzle.append(row)
        
        # Parse das restrições horizontais
        h_const = {}
        for r, line in enumerate(horiz_lines):
            items = line.split()
            for c, item in enumerate(items[1::2]):  # Pega apenas os símbolos de comparação
                if item == '<':
                    h_const[(r, c)] = 0  # Célula da esquerda é menor
                elif item == '>':
                    h_const[(r, c)] = 1  # Célula da esquerda é maior
                # 'x' é ignorado, pois indica ausência de restrição
        
        # Parse das restrições verticais
        v_const = {}
        for r, line in enumerate(vert_lines):
            items = line.split()
            for c, item in enumerate(items):
                if item == '^':
                    v_const[(r, c)] = 1  # Célula de cima é maior (aponta para cima)
                elif item == 'v':
                    v_const[(r, c)] = 0  # Célula de cima é menor (aponta para baixo)
                # 'x' é ignorado, pois indica ausência de restrição
        
        return puzzle, h_const, v_const
    
    except Exception as e:
        print(f"Erro ao processar o arquivo de entrada '{filename}': {e}")
        sys.exit(1)

def main():
    parser = argparse.ArgumentParser(description=f'Resolvedor de Futoshiki {SIZE}x{SIZE} com Quadrantes')
    parser.add_argument('--infile', type=str, required=True, help='Arquivo de entrada do puzzle')
    parser.add_argument('--outfile', type=argparse.FileType('w'), default=sys.stdout, help='Arquivo de saída da solução (padrão: stdout)')
    args = parser.parse_args()

    # Lê e processa o arquivo de entrada
    puzzle_list, h_const, v_const = parse_input_file(args.infile)

    # Informação sobre o puzzle
    print("--- Puzzle Inicial ---")
    printlst(puzzle_list, sys.stdout)
    print("\n--- Restrições H ---")
    print(h_const)
    print("\n--- Restrições V ---")
    print(v_const)
    print("\n----------------------")
    print("Resolvendo...")

    # Resolução
    solver = BackTracker(puzzle_list, h_const, v_const)
    solution_list = solver.solve()

    # Saída
    print("\n--- Solução ---")
    if solution_list is not None:
        printlst(solution_list, args.outfile)
        if args.outfile != sys.stdout:
            print(f"Solução escrita em: {args.outfile.name}")
            args.outfile.close()
    else:
        print("Não foi possível encontrar uma solução.")
        if args.outfile != sys.stdout:
            args.outfile.write("Sem solucao\n")
            args.outfile.close()

if __name__ == "__main__":
    main()