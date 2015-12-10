#lang racket

(require racket/gui)

; tamanho da altura e da largura, em blocos
(define BLOCKS_WIDTH 20)
(define BLOCKS_HEIGHT 20)

; tamanho em pixels de cada bloco
(define BLOCK_SIZE 16)

; lista com as posições da cobra
(define cobra (list (list 2 1) (list 1 1)))

; para onde a cobra está indo
(define direcao 'r)

; onde a pepsi está
(define pepsi (list 9 8))

; quantas pepsis tomou
(define pontos 0)

; move-bloco: inteiro inteiro -> lista
; objetivo: remove o último elemento da lista e adiciona um novo no começo dela
(define (move-bloco x y) 
  (reverse (append (cdr (reverse cobra)) (list(list x y)))))

; pega-cabeca: inteiro lista -> ?
; objetivo: pega o elemento no indice do arugmento posicao do primeiro elemento da lista
; exemplo:  (pega-cabeca 1 (list (list 4 5) (list 6 7))) -> 4
(define (pega-cabeca posicao lst)
  (list-ref (list-ref lst 0) posicao))

; draw-block: dc inteiro inteiro string -> void
; objetivo: desenha um bloco no objeto de desenho dado
;           o argumento color pode ser uma string ou um objeto de cor
(define (draw-block screen x y color) 
  (send screen set-brush color 'solid)
  (send screen draw-rectangle (* x BLOCK_SIZE) (* y BLOCK_SIZE) BLOCK_SIZE BLOCK_SIZE))

; move-cobra: simbolo -> void
; objetivo: move a cobra para posição dada('l, 'r, 'u ou 'd)
(define (move-cobra posicao)
  (case posicao
    ['l (set! cobra (move-bloco (- (pega-cabeca 0 cobra) 1) (pega-cabeca 1 cobra)))]
    ['r (set! cobra (move-bloco (+ (pega-cabeca 0 cobra) 1) (pega-cabeca 1 cobra)))]
    ['u (set! cobra (move-bloco (pega-cabeca 0 cobra) (- (pega-cabeca 1 cobra) 1)))]
    ['d (set! cobra (move-bloco (pega-cabeca 0 cobra) (+ (pega-cabeca 1 cobra) 1)))]))

; encostou-bloco: lista lista [número] [número] -> bool
; objetivo: verifica se a lista de pontos pepsi está na lista cobra
;           g é o índice de exclusão
(define (encostou-bloco cobra bloco [i 0] [g 666]) 
  ;(displayln g)
  (if (> (length cobra) i)  ; length (cobra) > i
    (if (and (not (= g i)) (and 
      (eq? (list-ref (list-ref cobra i) 0) (list-ref bloco 0)) ; verifica pelo x e pelo y
      (eq? (list-ref (list-ref cobra i) 1) (list-ref bloco 1)))) 
        #t
      (encostou-bloco cobra bloco (+ i 1) g))
    #f))

; cresce-cobra: -> void
; objetivo: faz a cobra crescer, copiando o último elemento da lista cobra, movendo a cobra para outra posiçao
;           e adicionando o último elemento pego anteriormente
(define cresce-cobra (lambda () 
  (define x (car (reverse cobra)))
  (set! pepsi (list (inexact->exact (round (* (random) (- BLOCKS_WIDTH 1)))) (inexact->exact (round (* (random) (- BLOCKS_HEIGHT 1)))) ))
  (move-cobra direcao)
  (set! pontos (add1 pontos))
  (set! cobra (append cobra (list x)))))

; restart: ->void
; objetivo: seta todas as variaveis para o estado original
(define restart (lambda()
  (set! direcao 'r)
  (set! pepsi (list 9 8))
  (set! cobra (list (list 2 1) (list 1 1)))
  (set! pontos 0)
))

; cria a janela para a cobra
(define frame (new frame% 
  [label "Cobra"]
  [width (* BLOCKS_WIDTH BLOCK_SIZE)]
  [height (* BLOCKS_HEIGHT BLOCK_SIZE)]))

; canvas-key: -> canvas%
(define (canvas-key frame) (class canvas%
  (define/override (on-char key-event)
    (cond ; todo: in_list? -> set direcao
      [(eq? (send key-event get-key-code) 'left) (set! direcao 'l)]
      [(eq? (send key-event get-key-code) 'right) (set! direcao 'r)]
      [(eq? (send key-event get-key-code) 'up) (set! direcao 'u)]
      [(eq? (send key-event get-key-code) 'down) (set! direcao 'd)]
      [(eq? (send key-event get-key-code) '#\r) (restart)]))
  (super-new [parent frame])))

; atualiza-cobra: -> void
; objetivo: funções para a atualização da tela e movimentação da cobra
(define atualiza-cobra (lambda () 
  (draw-block dc (list-ref pepsi 0) (list-ref pepsi 1) "blue") ; desenha a pepsi
  (cond [(encostou-bloco cobra pepsi) (cresce-cobra)] [else (move-cobra direcao)]) ; checa por colisão com a pepsi
  (send dc draw-text (number->string pontos) (-(* BLOCKS_WIDTH BLOCK_SIZE) 30) 10)
  (for ([block cobra]) (
    if (eq? block (car cobra)) 
      (draw-block dc (list-ref block 0) (list-ref block 1) "white") 
      (draw-block dc (list-ref block 0) (list-ref block 1) "white")))))

; lost-the-gaem: -> void
; objetivo: eventos que ocorrem ao perder o jogo
(define lost-the-gaem (lambda ()
  (send dc draw-text "you just lost the game" (- (round (/ (* BLOCKS_WIDTH BLOCK_SIZE) 2)) 110) (- (round (/ (* BLOCKS_HEIGHT BLOCK_SIZE) 2)) 20))
  (send dc draw-text "(press r to restart)" (- (round (/ (* BLOCKS_WIDTH BLOCK_SIZE) 2)) 100) (- (round (/ (* BLOCKS_HEIGHT BLOCK_SIZE) 2)) 0))
))

; instancia o canvas
(define canvas (
  new (canvas-key frame)))

; pega o drawing canvas
(define dc (send canvas get-dc))

; seta a fonte
(send dc set-font (make-object font% 12 'modern))
(send dc set-text-foreground "white")

; manda o objeto frame aparecer
(send frame show #t)

; loop com colisoes e frames
(define timer (new timer%
  [notify-callback (lambda()
    (send dc clear)
    (send dc set-brush "black" 'solid)        
    (send dc draw-rectangle 0 0 (* BLOCKS_WIDTH BLOCK_SIZE) (* BLOCKS_HEIGHT BLOCK_SIZE))                 
    
    (define colisao #f)
    (for ([block cobra]
         [j (in-naturals 0)])
      (cond 
            [(or (> (list-ref block 0) BLOCKS_WIDTH) (> 0 (list-ref block 0))) (set! colisao #t )]
            [(or (> (list-ref block 1) BLOCKS_HEIGHT) (> 0 (list-ref block 1))) (set! colisao #t)]
            [(eq? #f colisao) (set! colisao (eq? #t (encostou-bloco cobra block 0 j)))]))
    (if colisao (lost-the-gaem) (atualiza-cobra)))]
  [interval #f]))

(send timer start 100)