#lang racket
;librerias
(require (lib "graphics.ss" "graphics")) 
(open-graphics)

;nota
;para usar imágenes transparentes tienen que ser en formato png y fondo transparente
;(((draw-pixmap-posn "imagen.extension") ventana)(make-posn (position horizontal) (position vertical) "red")
;la imagen debe de estar en la misma carpeta en la que se encuentre el proyecto formato racket


;pasos
;::::::::::::::::::::::::::::::
;define y abre la "ventana""
(define ventana (open-viewport "ventana" 800 500))
;color de "ventana" 
((draw-viewport ventana) "black") 
;:::::::::::::::::::::

;define a "ventana2" (llevará el fondo)
(define ventana2 (open-pixmap "eso no importa!" 800 500 #|tamaño de la ventana|#))
;:::::::::::::
;adicionar fondo a ventana 2
((draw-pixmap ventana2) "presidente.jpg" (make-posn 0.0 0.0) "black")
;:::::::::
;color del fondo en "ventana2" (obcional)
;((draw-viewport ventana2) "color")
;::::::::::::::::::
;copiamos el contenido de una ventana2 a ventana
(copy-viewport ventana2 ventana)
;;;;;;;;;;;;;;;;;;;
;texto en pantalla
((draw-string ventana2) (make-posn 20 20)"By:Brian Ruiz Idarraga""red")
((draw-string ventana2) (make-posn 20 40)"     Hector Fabio Jimenes""red")
((draw-string ventana2) (make-posn 20 60)"Para salir presione enter""red")


  ;estructura de la imagen que se moverá
  ;posicion en horizontal
  (define x (random 725))  ;cambiar el valor utilizar                            
  ;posición en vertical    ;set!--> (set! x (nueva-posicion))
  (define y (random 430))
  ;velocidad de movimiento
  (define z 7)


;:::::::::::::::::::::::::::::::::::::::::::::::
;actualiza la ventana con el movimiento 
(define (nave lad)
 (begin
  (copy-viewport ventana2 ventana);actualizamos el fondo original   
  (cond     
    [(equal? lad 'l)                    
      (((draw-pixmap-posn "bigote.png") ventana)(make-posn x y) "red")]   
    [else (void)] ;ponemos la imagen que queremos mover en la pantalla   
   )
 )
)
;::::::::::::::::::::::::::::::::::::::::::::::::::
;cambia la posición de movimiento de la imagen editando x o y
(define (move# p)
(cond
 ;up 
  ((and (equal? p 'up)(>= y 10)#|limite ariva|#) (begin (set! y (- y z)) (nave 'l)))
  
 ;down  
  ((and (equal? p 'down)(<= y 430)#|limite abajo|#)  (begin (set! y (+ y z)) (nave 'l)) )      
 ;left
  [(and (equal? p 'left) (>= x 10)#|limite izquierda|#)
    (begin (set! x (- x z)) (nave 'l))]
 ;right
  [(and (equal? p 'right) (<= x 725) #|limite derecha|#)
   (begin (set! x (+ x z)) (nave 'l))]
  )
)
;::::::::::::::::::::::::::::::::::::::::::::::::::
;captura las teclas presionadas
(define (teclado press)
 (cond
   ;left
    [(equal? press 'left)
     (begin
       (move# 'left)
       (teclado (key-value (get-key-press ventana))))]
    ;right
    [(equal? press 'right)
     (begin
       (move# 'right)
       (teclado (key-value (get-key-press ventana))))]
    ;up
    [(equal? press 'up)
     (begin
       (move# 'up)
       (teclado (key-value (get-key-press ventana))))]
   ;down
    [(equal? press 'down)
     (begin
       (move# 'down)
       (teclado (key-value (get-key-press ventana))))]
    ;salir
    [(equal? press '#\return)
     (close-viewport ventana)]
    
 ;mas teclas: (enter= #\return), (espacio=#\space), (letras #\char).
 
    ;else
    [else (teclado (key-value (get-key-press ventana)))]      
  )
) 
;:::::::::::::::::::::::::::::::::::::::::::::::::::::
;inicia la captura de teclas
(teclado 'up)

