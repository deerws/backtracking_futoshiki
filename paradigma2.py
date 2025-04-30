import copy
import numpy as np  # Mantido por hábito, mas não estritamente necessário após refatoração
import argparse
import sys
import time
from collections import defaultdict # Útil para affected_cells

SIZE = 4  # Define o tamanho do tabuleiro
QUAD_SIZE = 2 # Define o tamanho do quadrante (2x2)

def listify(lst):
    """Converte linhas de texto em listas de inteiros/caracteres"""
    returnlst = []
    for item in lst:
        # Tenta converter para int, senão mantém como string (para restrições)
        # Remove strings vazias resultantes de múltiplos espaços
        processed = [int(x) if x.isdigit() or (x.startswith('-') and x[1:].isdigit()) else x
                     for x in item.split() if x]
        returnlst.append(processed)
    return returnlst

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
            # Usar lista de listas internamente, numpy não é essencial aqui
            self.puzzle = puzzle_list
            self.h_const = h_const
            self.v_const = v_const
            self.parent = parent
            self.children = [] # Manter rastreio pode ser útil para debug, mas não essencial para o solve

            self.domains = None # Será inicializado depois
            self.target = None # Próxima célula a preencher
            self.target_vals = [] # Valores a tentar para self.target
            self.target_index = 0 # Índice do valor atual em self.target_vals

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
                    if c > 0 and (r, c - 1) in self.h_const: # Restrição à esquerda
                        affected[coords].add((r, c - 1))
                    if c < self.size - 1 and (r, c) in self.h_const: # Restrição à direita
                        affected[coords].add((r, c + 1))

                    # 5. Desigualdade Vertical (célula adjacente)
                    if r > 0 and (r - 1, c) in self.v_const: # Restrição acima
                        affected[coords].add((r - 1, c))
                    if r < self.size - 1 and (r, c) in self.v_const: # Restrição abaixo
                        affected[coords].add((r + 1, c))

            return affected

        def initialize_domains(self):
            """Inicializa os domínios de todas as células"""
            self.domains = [[set(range(1, self.size + 1)) if self.puzzle[r][c] == 0 else {self.puzzle[r][c]}
                             for c in range(self.size)] for r in range(self.size)]

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
                            return False # Inconsistência inicial
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
                            # print(f"DEBUG: Inconsistencia apos remover {assigned_value} de ({ar},{ac}) por unicidade com ({r},{c})")
                            return False # Domínio vazio -> Inconsistência

            # 2. Propagação de Desigualdades (para vizinhos *não preenchidos*)
            # Horizontal - Vizinho à Direita (r, c+1)
            if c < self.size - 1 and (r, c) in self.h_const:
                ar, ac = r, c + 1
                if self.puzzle[ar][ac] == 0: # Se vizinho não preenchido
                    ineq = self.h_const[(r, c)] # Restrição entre (r,c) e (r,c+1)
                    vals_to_remove = set()
                    if ineq == 1: # (r,c) > (r,c+1)  => assigned_value > vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    else:         # (r,c) < (r,c+1)  => assigned_value < vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            # Horizontal - Vizinho à Esquerda (r, c-1)
            if c > 0 and (r, c - 1) in self.h_const:
                ar, ac = r, c - 1
                if self.puzzle[ar][ac] == 0:
                    ineq = self.h_const[(r, c - 1)] # Restrição entre (r,c-1) e (r,c)
                    vals_to_remove = set()
                    if ineq == 1: # (r,c-1) > (r,c)  => vizinho > assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    else:         # (r,c-1) < (r,c)  => vizinho < assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            # Vertical - Vizinho Abaixo (r+1, c)
            if r < self.size - 1 and (r, c) in self.v_const:
                ar, ac = r + 1, c
                if self.puzzle[ar][ac] == 0:
                    ineq = self.v_const[(r, c)] # Restrição entre (r,c) e (r+1,c)
                    vals_to_remove = set()
                    if ineq == 1: # (r,c) > (r+1,c) (^ na interface) => assigned_value > vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    else:         # (r,c) < (r+1,c) (v na interface) => assigned_value < vizinho
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            # Vertical - Vizinho Acima (r-1, c)
            if r > 0 and (r - 1, c) in self.v_const:
                ar, ac = r - 1, c
                if self.puzzle[ar][ac] == 0:
                    ineq = self.v_const[(r - 1, c)] # Restrição entre (r-1,c) e (r,c)
                    vals_to_remove = set()
                    if ineq == 1: # (r-1,c) > (r,c) (^ na interface) => vizinho > assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v <= assigned_value}
                    else:         # (r-1,c) < (r,c) (v na interface) => vizinho < assigned_value
                        vals_to_remove = {v for v in self.domains[ar][ac] if v >= assigned_value}
                    if vals_to_remove:
                        self.domains[ar][ac] -= vals_to_remove
                        if not self.domains[ar][ac]: return False

            return True # Estado consistente após propagação

        def choose_next_variable(self):
            """Escolhe próxima variável usando MRV + Degree"""
            min_domain_size = float('inf')
            candidates = []

            for r in range(self.size):
                for c in range(self.size):
                    if self.puzzle[r][c] == 0: # Apenas células não atribuídas
                        domain_size = len(self.domains[r][c])
                        if domain_size == 0: # Inconsistência encontrada
                            self.target = (-1,-1) # Sinaliza erro/beco sem saída
                            return
                        if domain_size < min_domain_size:
                            min_domain_size = domain_size
                            candidates = [(r, c)]
                        elif domain_size == min_domain_size:
                            candidates.append((r, c))

            if not candidates: # Não há mais células vazias
                self.target = None # Sinaliza que o puzzle está completo
                return

            # Degree Heuristic (desempate) - Conta vizinhos *não atribuídos* afetados
            if len(candidates) > 1:
                max_degree = -1
                best_candidate = candidates[0]
                for r, c in candidates:
                    # Grau = número de vizinhos não atribuídos no grafo de restrições
                    # Usamos o cache _affected_cells_cache para eficiência
                    degree = sum(1 for ar, ac in self._affected_cells_cache[(r,c)]
                                 if self.puzzle[ar][ac] == 0)
                    if degree > max_degree:
                        max_degree = degree
                        best_candidate = (r, c)
                self.target = best_candidate
            else:
                self.target = candidates[0] # Único candidato MRV

        def get_ordered_values(self):
            """Ordena valores do domínio do target usando LCV"""
            if self.target is None or self.target == (-1,-1):
                 self.target_vals = []
                 return

            r, c = self.target
            values = list(self.domains[r][c])

            # LCV - Conta quantos valores seriam removidos dos domínios dos vizinhos
            def count_conflicts(val_to_try):
                conflicts = 0
                # Simula o impacto nos vizinhos (sem deepcopy, apenas contando)
                for ar, ac in self._affected_cells_cache[(r,c)]:
                    if self.puzzle[ar][ac] == 0: # Se vizinho não atribuído
                        # Verifica unicidade linha/coluna/quadrante
                        in_same_row = (r == ar)
                        in_same_col = (c == ac)
                        in_same_quad = (r // self.quad_size == ar // self.quad_size) and \
                                       (c // self.quad_size == ac // self.quad_size)
                        if (in_same_row or in_same_col or in_same_quad) and val_to_try in self.domains[ar][ac]:
                             conflicts += 1 # Contaria como 1 remoção potencial

                        # Verifica desigualdades (simplificado: só conta se o valor está no domínio do vizinho)
                        # Uma contagem mais precisa exigiria verificar quantos valores *específicos* são eliminados pela desigualdade
                        # Mas para LCV, contar se o valor está no domínio vizinho já dá uma ideia
                        # Horizontal
                        elif c + 1 == ac and (r,c) in self.h_const and val_to_try in self.domains[ar][ac]: conflicts +=1
                        elif c - 1 == ac and (ar,ac) in self.h_const and val_to_try in self.domains[ar][ac]: conflicts +=1
                        # Vertical
                        elif r + 1 == ar and (r,c) in self.v_const and val_to_try in self.domains[ar][ac]: conflicts +=1
                        elif r - 1 == ar and (ar,ac) in self.v_const and val_to_try in self.domains[ar][ac]: conflicts +=1

                return conflicts

            # Ordena: menos conflitos primeiro
            self.target_vals = sorted(values, key=count_conflicts)
            self.target_index = 0

        def is_complete(self):
            """Verifica se o puzzle está completo (sem zeros)"""
            return all(self.puzzle[r][c] != 0 for r in range(self.size) for c in range(self.size))

        # isValid() não é mais necessária, a consistência é mantida por _propagate

    # --- Fim da classe Board ---

    def __init__(self, initial_puzzle_list, h_const, v_const):
        """Inicializa o resolvedor com o puzzle e restrições"""
        # Cria o estado inicial (raiz da árvore de busca)
        self.root = self.Board(initial_puzzle_list, h_const, v_const)

    def solve(self):
        """Executa o algoritmo de backtracking com propagação de restrições"""
        
        # Inicializa domínios e aplica consistência inicial
        if not self.root.initialize_domains():
             print("Erro na inicialização dos domínios.") # Não deve acontecer
             return None
        if not self.root.apply_initial_consistency():
            print("Puzzle inicial inconsistente.")
            return None

        curr = self.root # Começa na raiz
        node_visits = 0
        max_node_visits = 2000000 # Aumentado um pouco
        start_time = time.time()

        while True:
            node_visits += 1
            if node_visits > max_node_visits:
                print(f"Limite de {max_node_visits} visitas atingido.")
                # printlst(curr.puzzle, sys.stdout) # Opcional: mostrar último estado
                return None

            # Feedback de progresso
            if node_visits % 50000 == 0:
                elapsed = time.time() - start_time
                print(f"Visitas: {node_visits}... (Tempo: {elapsed:.2f}s)")

            # 1. Verifica se o estado atual é uma solução completa
            if curr.is_complete():
                elapsed = time.time() - start_time
                print(f"\nSolução encontrada! Visitas: {node_visits} (Tempo: {elapsed:.2f}s)")
                return curr.puzzle # Retorna a solução

            # 2. Escolhe a próxima variável (célula) e ordena seus valores
            if curr.target is None: # Só escolhe se ainda não foi escolhido para este nó
                 curr.choose_next_variable()
                 curr.get_ordered_values()

            # 3. Verifica se há um caminho a seguir (variável escolhida e valores restantes)
            if curr.target != (-1,-1) and curr.target_vals and curr.target_index < len(curr.target_vals):
                # --- Tentativa (Descer na árvore) ---
                
                # Pega o próximo valor a tentar para a variável escolhida
                val_to_try = curr.target_vals[curr.target_index]
                target_r, target_c = curr.target

                # Cria um *novo* estado filho (Board) fazendo cópias profundas
                # É importante copiar ANTES de modificar
                new_puzzle = copy.deepcopy(curr.puzzle)
                new_domains = copy.deepcopy(curr.domains)

                # Atribui o valor no novo estado
                new_puzzle[target_r][target_c] = val_to_try
                new_domains[target_r][target_c] = {val_to_try} # Fixa o domínio

                # Cria o nó filho
                child = self.Board(new_puzzle, self.root.h_const, self.root.v_const, curr)
                child.domains = new_domains # Atribui os domínios copiados e modificados
                child.target = None # Filho precisará escolher sua própria variável
                # child._affected_cells_cache é herdado/recalculado no init do Board, ok.

                # Propaga as restrições a partir da nova atribuição no filho
                # Se a propagação for bem-sucedida (não encontrar inconsistência)...
                if child._propagate_constraints(target_r, target_c, val_to_try):
                    # ...avança para o estado filho
                    # curr.children.append(child) # Opcional: manter árvore explícita
                    curr = child
                else:
                    # ...se a propagação falhou (inconsistência),
                    # descarta este valor e tenta o próximo no NÓ ATUAL.
                    curr.target_index += 1

            else:
                # --- Backtrack (Subir na árvore) ---
                # Ocorre se:
                # - Nenhuma variável pôde ser escolhida (target é None mas não completo, ou (-1,-1))
                # - Não há mais valores a tentar para a variável atual (target_index >= len(target_vals))

                if curr.parent is None:
                    # Chegou de volta à raiz e não há mais opções -> Sem solução
                    elapsed = time.time() - start_time
                    print(f"\nBacktrack até a raiz sem solução. Visitas: {node_visits} (Tempo: {elapsed:.2f}s)")
                    return None

                # Sobe para o pai
                curr = curr.parent
                # Prepara para tentar o PRÓXIMO valor do pai na próxima iteração
                curr.target_index += 1
                # Reseta target/target_vals do pai para forçar re-escolha se necessário? Não,
                # só incrementa o índice do valor que estava sendo tentado.

# --- Fim da classe BackTracker ---

def gen_constraints(horiz_lines, vert_lines):
    """Gera dicionários de restrições H e V a partir das listas de símbolos"""
    HConstraints = {}
    VConstraints = {}
    constraint_dict = {'^': 1, '>': 1, '<': 0, 'v': 0} # 1: Maior, 0: Menor

    # Restrições Horizontais (entre colunas c e c+1)
    for r in range(len(horiz_lines)):
        line = horiz_lines[r]
        # Espera-se SIZE-1 símbolos por linha horizontal
        for c in range(len(line)):
            symbol = line[c]
            if symbol in constraint_dict:
                HConstraints[(r, c)] = constraint_dict[symbol] # Chave é a célula à ESQUERDA

    # Restrições Verticais (entre linhas r e r+1)
    for r in range(len(vert_lines)):
        line = vert_lines[r]
        # Espera-se SIZE símbolos por linha vertical
        for c in range(len(line)):
            symbol = line[c]
            if symbol in constraint_dict:
                 VConstraints[(r, c)] = constraint_dict[symbol] # Chave é a célula ACIMA

    return HConstraints, VConstraints

def main():
    parser = argparse.ArgumentParser(description=f'Resolvedor de Futoshiki {SIZE}x{SIZE} com Quadrantes')
    parser.add_argument('--infile', type=str, required=True, help='Arquivo de entrada do puzzle')
    parser.add_argument('--outfile', type=argparse.FileType('w'), default=sys.stdout, help='Arquivo de saída da solução (padrão: stdout)')
    args = parser.parse_args()

    # --- Leitura e Parsing do Arquivo ---
    try:
        with open(args.infile, 'r') as f:
            # Lê todas as linhas, remove espaços extras nas pontas, ignora linhas vazias
            lines = [line.strip() for line in f if line.strip()]

        # Valida número de linhas (espera 4 puzzle + 4 H + 4 V = 12 no formato compacto)
        # Ou 4 puzzle + 4 H + 3 V ? O formato de input precisa ser consistente.
        # Vamos assumir 4 puzzle, 4 H, 4 V por enquanto, total 12 linhas.
        if len(lines) < SIZE * 3:
             raise ValueError(f"Formato de arquivo inválido. Esperadas {SIZE*3} linhas não vazias.")

        puzzle_lines = lines[0:SIZE]
        horiz_lines_raw = lines[SIZE : SIZE * 2]
        vert_lines_raw = lines[SIZE * 2 : SIZE * 3] # Assume 4 linhas V

        # Converte puzzle para lista de listas de inteiros
        puzzle_list = []
        for r in range(SIZE):
             row_str = puzzle_lines[r].split()
             if len(row_str) != SIZE: raise ValueError(f"Linha {r+1} do puzzle não tem {SIZE} elementos.")
             puzzle_list.append([int(cell) if cell.isdigit() else 0 for cell in row_str])

        # Processa restrições (apenas pega os símbolos)
        horiz_constraints_symbols = []
        for r in range(SIZE):
             symbols = [s for s in horiz_lines_raw[r].split() if s in ['<', '>']]
             # Espera-se SIZE-1 símbolos válidos por linha H
             if len(symbols) > SIZE -1: print(f"Aviso: Mais de {SIZE-1} simbolos H na linha {r+SIZE+1}")
             horiz_constraints_symbols.append(symbols[:SIZE-1]) # Garante tamanho maximo

        vert_constraints_symbols = []
        for r in range(SIZE): # Ler 4 linhas V
             symbols = [s for s in vert_lines_raw[r].split() if s in ['v', '^']]
              # Espera-se SIZE símbolos válidos por linha V
             if len(symbols) > SIZE : print(f"Aviso: Mais de {SIZE} simbolos V na linha {r+SIZE*2+1}")
             vert_constraints_symbols.append(symbols[:SIZE]) # Garante tamanho maximo
             
        # Ajuste: gen_constraints espera as linhas como estão, vamos passar as listas de símbolos
        # Modificar gen_constraints para aceitar listas de símbolos
        # OU formatar de volta? Mais fácil modificar gen_constraints
        
        # Gera dicionários H e V
        h_const, v_const = gen_constraints(horiz_constraints_symbols, vert_constraints_symbols)

    except Exception as e:
        print(f"Erro ao processar o arquivo de entrada '{args.infile}': {e}")
        sys.exit(1)

    # --- Resolução ---
    print("--- Puzzle Inicial ---")
    printlst(puzzle_list, sys.stdout)
    print("\n--- Restrições H ---")
    print(h_const)
    print("\n--- Restrições V ---")
    print(v_const)
    print("\n----------------------")
    print("Resolvendo...")

    solver = BackTracker(puzzle_list, h_const, v_const)
    solution_list = solver.solve() # solve retorna lista de listas

    # --- Saída ---
    print("\n--- Solução ---")
    if solution_list is not None:
        printlst(solution_list, args.outfile) # Usa printlst para formatar
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