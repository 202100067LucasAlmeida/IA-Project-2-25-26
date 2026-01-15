#|
# Projeto 2 - Inteligência Artificial
#
# Ficheiro destinado a implementar os métodos da jogada de cada jogador (AI contra jogador).
#
# Docente > Joaquim Filipe
#
# Alunos:
# > Danilo Victor, 202300224
# > Jean Oliveira, 202300095
# > Lucas Almeida, 202100067
#
|#

(defvar *jogador2* -1)
(defvar *jogador1* 1)
(defparameter *infinity* 9999999999999999)

(defun negamax (no altura jogador tempo &optional(cor 1) (alfa (- *infinity*)) (beta *infinity*) &aux(operadores (operadores)))
  "Implementação do algoritmo Negamax com poda Alfa-Beta"
  (let ((sucessores-no (sucessores no operadores jogador altura)))
        (cond ((null sucessores-no) (* cor (no-heuristica no)))
              (t (negamax-recursivo sucessores-no alfa beta altura jogador (- *infinity*) tempo cor))
        )
  )
)

(defun negamax-recursivo (sucessores-no alfa beta altura jogador melhor-valor tempo cor &aux(no (car sucessores-no)))
  (cond ((or (>= alfa beta) (null sucessores-no)) melhor-valor)
        (t (let* ((novo-melhor-valor (max melhor-valor (- (negamax no altura (- jogador) tempo (- cor) (- beta) (- alfa)))))
                  (novo-alfa (max alfa novo-melhor-valor))
                  ;(aux (format t"~a~%" sucessores-no))
                  )
              (negamax-recursivo (cdr sucessores-no) novo-alfa beta altura jogador novo-melhor-valor tempo cor)
            )
        )
  )
)

(defun sucessores (no operadores jogador profundidade &key(heuristica 'heuristica))
  "Gera a lista de sucessores do nó"
  (cond ((null no) nil)
        ((null operadores) nil)
        ((= (no-profundidade no) profundidade) nil)
        ((no-solucaop no) nil)
        (t (let ((todos-nos (remove-if #'null (apply #'append 
                 (mapcar #'(lambda (operador) (novo-sucessor no operador jogador heuristica)) operadores)))))
              (ordenar-nos todos-nos)
           )
        )
  )
)

(defun novo-sucessor (no operador jogador heuristica &optional(x 1) (y 1))
  "Gera um novo nó, a partir do operador e nó pai"
  (cond ((> x 7) nil)
        ((> y 7) (novo-sucessor no operador jogador heuristica (1+ x)))
        (t (let ((novo-tabuleiro (funcall operador jogador x y (no-estado no))))
             (cond ((null novo-tabuleiro) (novo-sucessor no operador jogador heuristica x (1+ y)))
                 (t (cons (cria-no novo-tabuleiro heuristica jogador (1+ (no-profundidade no)) no) (novo-sucessor no operador jogador heuristica x (1+ y))))
             )
           )
        )
  )
)

(defun ordenar-nos (nos)
  "Ordena os nós de acordo com o valor de cada um"
  (cond ((null nos) nil)
        (t (let ((min-no (ordenar-nos-recursivo (first nos) (cdr nos))))
                (cons min-no (ordenar-nos (remove min-no nos)))
           )
        )
  )
)

(defun ordenar-nos-recursivo (no nos)
  (cond ((null nos) no)
        ((< (no-heuristica (car nos)) (no-heuristica no)) (ordenar-nos-recursivo (car nos) (cdr nos)))
        (t (ordenar-nos-recursivo no (cdr nos)))
  )
)

(defun heuristica (tabuleiro jogador)
  "Calcula a heurística de acordo com a distancia de um peão até a vitória e a quantidade de peões, de ambos os jogadores"
  (float (/ (+ (* (- (distancia-vitoria tabuleiro (- jogador)) (distancia-vitoria tabuleiro jogador)) 3) 
    (* (- (quantidade-peao tabuleiro (- jogador)) (quantidade-peao tabuleiro jogador)) 4)) 10))
)

(defun jogada-humano (tabuleiro jogada jogador x y)
  (funcall jogada jogador x y tabuleiro)
)