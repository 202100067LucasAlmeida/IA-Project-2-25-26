#|
# Projeto 2 - Inteligência Artificial
#
# Ficheiro destinado a implementar funções de auxiliação, definição dos operadores e heurísticas.
#
# Docente > Joaquim Filipe
#
# Alunos:
# > Danilo Victor, 202300224
# > Jean Oliveira, 202300095
# > Lucas Almeida, 202100067
#
# > Nota: Funções assinaladas com (*) são funções definidas além do enunciado.
|#

(defvar *jogador2* -1)
(defvar *jogador1* 1)
(defconstant +infinity+ :infinity)
(defconstant -infinity+ :-infinity)


;;; Tabuleiros

(defun tabuleiro-teste (&aux (J1 *jogador1*) (J2 *jogador2*))
  "Tabuleiro de teste sem nenhuma jogada realizada"
  `((nil nil ,J1 ,J1 ,J1 nil nil)
    (nil nil ,J1 ,J1 ,J1 nil nil)
    ( 0   0   0   0   0   0   0 )
    ( 0   0   0   0   0   0   0 )
    ( 0   0   0   0   0   0   0 )
    (nil nil ,J2 ,J2 ,J2 nil nil)
    (nil nil ,J2 ,J2 ,J2 nil nil))
)

(defun no-teste ()
  "Cria um nó para testes"
  (list (tabuleiro-teste)  0 0 nil)
)


;;; Seletores Tabuleiro

;; Linha
(defun linha (n tabuleiro)
  "Retorna a linha n do tabuleiro"
  (cond ((posicao-validap n) (nth (1- n) tabuleiro))
             (t nil) 
   )
)

;; Coluna
(defun coluna (n tabuleiro)
  "Retorna a coluna n do tabuleiro"
  (cond ((posicao-validap n) (mapcar #'(lambda (x) (nth (1- n) x)) tabuleiro))
             (t nil)
   )
)

;; Célula
(defun celula (x y tabuleiro)
  "Retorna a célula (x, y) do tabuleiro"
  (cond ((and (posicao-validap x) (posicao-validap y)) (nth (1- y) (nth (1- x) tabuleiro)))
             (t nil)
   )
)


;;; Nós

;; Construtor
(defun cria-no (tabuleiro heuristica &optional (p 0) (pai nil))
  "Criar um nó com o estado do tabuleiro sua profundidade e seu nó pai"
  (list tabuleiro p (funcall heuristica tabuleiro) pai)
)


;;; Seletores Nó

;; No-estado
(defun no-estado (no)
  "Ver o estado do tabuleiro"
  (first no)
)

;; No-profundidade
(defun no-profundidade (no)
  "Ver a profundidade do nó"
  (second no)
)

;; No-pai
(defun no-pai (no)
  "Ver o nó pai do nó"
  (fourth no)
)

;; No-heuristica
(defun no-heuristica (no)
  "Ver a heuristica do nó"
  (third no)
)

;; No-valor
(defun no-valor (no)
  "Calcula o valor do nó"
  (+ (no-profundidade no) (no-heuristica no))
)

(defun no-solcaop (no)
  "Verifica se o nó é um nó solução"
  (cond ((or (contains (first (no-estado no)) *jogador2*))
        (contains (second (no-estado no)) *jogador2*)
        (contains (sixth (no-estado no)) *jogador1*)
        (contains (seventh (no-estado no)) *jogador1*) t)
        (t nil)
  )
)


;; Funções Auxiliares

;; Celula-validap
(defun celula-validap (x y tabuleiro)
  "Determina se a célula (x, y) do tabuleiro é válida (!= nil)"
  (cond ((null (celula x y tabuleiro)) nil)
             (t t)
  )
)

;; Substituir-posicao
(defun substituir-posicao (n linha x)
  "Substitui o indice n da linha por x"
  (cond ((or (not (posicao-validap n)) (null (nth (1- n) linha))) nil)
             ((and (/= x -1) (/= x 0) (/= x 1)) nil)
             ((= n 1) (cons x (rest linha)))
             (t (cons (first linha) (substituir-posicao (1- n) (rest linha) x)))
  )
)

;; Substituir
(defun substituir (l n tabuleiro x)
  "Substituir o indice n da linha l do tabuleiro por x"
  (cond ((or (null tabuleiro)) nil)
             ((= l 1) (cons (substituir-posicao n (first tabuleiro) x) (rest tabuleiro)))
             (t (cons (first tabuleiro) (substituir (1- l) n (rest tabuleiro) x)))
  )
)

;; Posição Válida (*)
(defun posicao-validap (x)
  "Validar se a posição x é válida no tabuleiro 7x7"
  (cond ((or (< x 1) (> x 7)) nil)
             (t t)
  )
)

(defun contains (linha jogador)
  (some (lambda (x) (= x jogador)) linha)
)


;; Operadores

;; Todos os Operadores
(defun operadores ()
  "Cria uma lista com todos os operadores do jogo"
  (list 'operador-cd 'operador-ce 'operador-cc 'operador-cb 'operador-d 'operador-e 'operador-c 'operador-b)
)

;; Captura Direita
(defun operador-cd (jogador x y tabuleiro)
  "Realizar uma captura de pino à direita"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap x (+ y 2) tabuleiro))) nil)
             ((not (equal (celula x (+ y 2) tabuleiro) 0)) nil)
             ((or (not (equal (celula x y tabuleiro) jogador))
                  (equal (celula x (1+ y) tabuleiro) jogador)) nil)
             (t (substituir x y (substituir x (1+ y) (substituir x (+ y 2) tabuleiro jogador) 0) 0))
   )
)

;; Captura Esquerda
(defun operador-ce (jogador x y tabuleiro)
  "Realizar uma captura de pino à esquerda"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap x (- y 2) tabuleiro))) nil)
             ((not (equal (celula x (- y 2) tabuleiro) 0)) nil)
             ((or (not (equal (celula x y tabuleiro) jogador))
                  (equal (celula x (1- y) tabuleiro) jogador)) nil)
             (t (substituir x y (substituir x (1- y) (substituir x (- y 2) tabuleiro jogador) 0) 0))
   )
)

;; Captura Cima
(defun operador-cc (jogador x y tabuleiro)
  "Realizar uma captura de pino à cima"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap (- x 2) y tabuleiro))) nil)
             ((not (equal (celula (- x 2) y tabuleiro) 0)) nil)
             ((or (not (equal (celula x y tabuleiro) jogador))
                  (equal (celula (1- x) y tabuleiro) jogador)) nil)
             (t (substituir x y (substituir (1- x) y (substituir (- x 2) y tabuleiro jogador) 0) 0))
   )
)

;; Captura Baixo
(defun operador-cb (jogador x y tabuleiro)
  "Realizar uma captura de pino à baixo"
  (cond ((null tabuleiro) nil)
             ((or (not (celula-validap x y tabuleiro))
                  (not (celula-validap (+ x 2) y tabuleiro))) nil)
             ((not (equal (celula (+ x 2) y tabuleiro) 0)) nil)
             ((or (not (equal (celula x y tabuleiro) jogador))
                  (equal (celula (1+ x) y tabuleiro) jogador)) nil)
             (t (substituir x y (substituir (1+ x) y (substituir (+ x 2) y tabuleiro jogador) 0) 0))
   )
)

;; Andar Direita
(defun operador-d (jogador x y tabuleiro)
  "Realiza movimento para a direita com o pino"
  (cond ((null tabuleiro) nil)
        ((or (not (celula-validap x y tabuleiro))
             (not (celula-validap x (1+ y) tabuleiro))) nil)
        ((not (equal (celula x y tabuleiro) jogador)) nil)
        ((not (equal (celula x (1+ y) tabuleiro) 0)) nil)
        (t (substituir x y (substituir x (1+ y) tabuleiro jogador) 0))
  )
)

;; Andar Esquerda
(defun operador-e (jogador x y tabuleiro)
  "Realiza movimento para a direita com o pino"
  (cond ((null tabuleiro) nil)
        ((or (not (celula-validap x y tabuleiro))
             (not (celula-validap x (1- y) tabuleiro))) nil)
        ((not (equal (celula x y tabuleiro) jogador)) nil)
        ((not (equal (celula x (1- y) tabuleiro) 0)) nil)
        (t (substituir x y (substituir x (1- y) tabuleiro jogador) 0))
  )
)

;; Andar Cima
(defun operador-c (jogador x y tabuleiro)
  "Realiza movimento para a direita com o pino"
  (cond ((null tabuleiro) nil)
        ((or (not (celula-validap x y tabuleiro))
             (not (celula-validap (1- x) y tabuleiro))) nil)
        ((not (equal (celula x y tabuleiro) jogador)) nil)
        ((not (equal (celula (1- x) y tabuleiro) 0)) nil)
        (t (substituir x y (substituir (1- x) y tabuleiro jogador) 0))
  )
)

;; Andar Baixo
(defun operador-b (jogador x y tabuleiro)
  "Realiza movimento para a direita com o pino"
  (cond ((null tabuleiro) nil)
        ((or (not (celula-validap x y tabuleiro))
             (not (celula-validap (1+ x) y tabuleiro))) nil)
        ((not (equal (celula x y tabuleiro) jogador)) nil)
        ((not (equal (celula (1+ x) y tabuleiro) 0)) nil)
        (t (substituir x y (substituir (1+ x) y tabuleiro jogador) 0))
  )
)