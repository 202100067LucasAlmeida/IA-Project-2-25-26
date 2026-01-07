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

(defun negamax (no alfa beta altura corte jogador tempo &optional(melhor-jogada nil) (start-time0))
  (cond (())
        ((= tempo (get-internal-real-time)) melhor-jogada)
        ((or (= 0 altura) (= (no-profundidade no) altura)) (* (no-heuristica no) corte))
        ()
  )
)

(defun sucessores (no jogador)
  ()
)