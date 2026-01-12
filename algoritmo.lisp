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
(defconstant +infinity+ :infinity)
(defconstant -infinity+ :-infinity)

(defun negamax (no alfa beta altura jogador tempo &optional(nos-abertos nil) (nos-fechados nil) &aux(operadores (operadores)))
  "Implementação do algoritmo Negamax com poda Alfa-Beta"
  (negamax-recursivo alfa beta altura jogador (cons nos-abertos no) nos-fechados operadores)
)

(defun negamax-recursivo (alfa beta altura jogador nos-abertos nos-fechados)
  (cond ((null nos-abertos) nil)
        (t (let* ((no (car nos-abertos))
                 (lista-sucessores (sucessores no operadores jogador :profundidade altura)))
              (cond ((lista-sucessores) (negamax-recursivo alfa beta altura jogador 
                                        (append lista-sucessores (cdr nos-abertos)) (cons (car lista-aberto) lista-fechado)))
                    (t ())
              )
            )
        
        )
  )
)


(defun sucessores (no operadores jogador &key(profundidade)(heuristica 'heuristica))
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
  "Calcula a heurística de acordo com a quantidade de movimentos possiveis no tabuleiro"
  (+ ())
)