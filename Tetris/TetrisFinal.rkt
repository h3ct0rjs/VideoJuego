;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TetrisFinal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
 (require (lib "graphics.ss" "graphics"))
(open-graphics)
(define tetris (open-viewport "tetris"  (make-posn 280 500)))


;***************************************************************************
;Desarrollador Desconocido, Aporte de Christian Daniel Nuñez Mejia
;plataforma.utp.edu.co
;Proyecto Final de Programacion 1. UTP 2015-2.
;***************************************************************************

;----------------------------DEFINICIONES NECESARIAS--------------------------------
(define c (ready-key-press tetris));espera que llegue algo por teclado


;---------------VECTORES DE LA REJILLA-----------------------------------------------
;me crea los vectores que se asocian a la rejilla
(define mt0 (make-vector 14))
(define mt1 (make-vector 14))
(define mt2 (make-vector 14))
(define mt3 (make-vector 14))
(define mt4 (make-vector 14))
(define mt5 (make-vector 14))
(define mt6 (make-vector 14))
(define mt7 (make-vector 14))
(define mt8 (make-vector 14))
(define mt9 (make-vector 14))
(define mt10 (make-vector 14))
(define mt11 (make-vector 14))
(define mt12 (make-vector 14))
(define mt13 (make-vector 14))
(define mt14 (make-vector 14))
(define mt15 (make-vector 14))
(define mt16 (make-vector 14))
(define mt17 (make-vector 14))
(define mt18 (make-vector 14))
(define mt19 (make-vector 14))
(define mt20 (make-vector 14))
(define mt21 (make-vector 14))
(define mt22 (make-vector 14))
(define mt23 (make-vector 14))
(define mt24 (make-vector 14))

;-----------------------------------------------------------------------------------------------
;----------------------MASCARAS NECESARIAS-------------------------------------

;recibe un numeroy  arroja el vector dependiendo del numero
;(mascaravec-num 1)=> mt1 ;;este es el vector y asi tengo control en la parte vertical del tetris
(define(mascaravec-num num)
  (if (and (>= num 0) (<= num 24))
      (if (= num 0)
          mt0
          (if (= num 1)
              mt1
              (if (= num 2)
                  mt2
                  (if (= num 3)
                      mt3
                      (if (= num 4)
                          mt4
                          (if (= num 5)
                              mt5
                              (if (= num 6)
                                  mt6
                                  (if (= num 7)
                                      mt7
                                      (if (= num 8)
                                          mt8
                                          (if (= num 9)
                                              mt9
                                              (if (= num 10)
                                                  mt10
                                                  (if (= num 11)
                                                      mt11
                                                      (if (= num 12)
                                                          mt12
                                                          (if (= num 13)
                                                              mt13
                                                              (if (= num 14)
                                                                  mt14
                                                                  (if (= num 15)
                                                                      mt15
                                                                      (if (= num 16)
                                                                          mt16
                                                                          (if (= num 17)
                                                                              mt17
                                                                              (if (= num 18)
                                                                                  mt18
                                                                                  (if (= num 19)
                                                                                      mt19
                                                                                      (if (= num 20)
                                                                                          mt20
                                                                                          (if (= num 21)
                                                                                              mt21
                                                                                              (if (= num 22)
                                                                                                  mt22
                                                                                                  (if (= num 23)
                                                                                                      mt23
                                                                                                      (if (= num 24)
                                                                                                          mt24
                                                                                                          )))))))))))))))))))))))))))
      
                                                                                              


;esta funcion examina todos los vectores que forman la rejilla y si encuentra una poscion con un numero
;0 la pinta blanca y si encuentra una posicion con un numero 1 la pinta de color negra

;usa la funcion mascaravec-num para volver la variable num en los vectores que forman la rejilla para poder de manera recursiva
; hacer un escaneo en toda la rejilla y asi dibujar negro si es uno y blanco si es 0
(define (dibujar-rejilla num pos )
  (if (and (< pos 14)(<= num 24))
      (if (=  0 (vector-ref (mascaravec-num num) pos))
          (begin
     ((draw-solid-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20 "white")
     ((draw-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20 "black")
      (dibujar-rejilla num  (+ 1 pos ))
      )
          (if (= (vector-ref (mascaravec-num num) pos) 1)
              (begin
                
      ((draw-solid-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20 )
      ((draw-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20 "white")
      
      (dibujar-rejilla  num  (+ 1 pos ))
      )
              )
      )
      (if (<= num 24)
          (dibujar-rejilla  (+ num 1)  0)
          )
      )
  )

;#######################################################

;EFECTO DE BORRADO DE LINEA CUANDO ESTA SE LLENA
;borra la linea que esta completa y recursivamente acomoda
;las filas superiores de la rejilla uno hacia abajo para hacer el efecto de borrado de linea
(define (redibuja num pos)
  (if (not (= 0 num))
  (if(< pos 14)
     (begin
     (vector-set! (mascaravec-num num) pos (vector-ref (mascaravec-num ( - num 1 )) pos))
     (redibuja  num (+ 1 pos))
     )
    (redibuja (- num 1) 0)
     )
    (dibujar-rejilla 0 0 )
  )
  
  )
;Examina que linea esta llena y y llama a la funcion que borra la linea y baja las superiores
;hace el escaneo de abajo hacia arriba
(define(examinar-puntos num pos ct)
  (if (> num 0)
      (if (= ct 14)
          (begin
           (redibuja num 0)
            (examinar-puntos (- num 1) 0 1)
            )
          (if (= (vector-ref (mascaravec-num num) pos) 1)
              (examinar-puntos  num (+ 1 pos) (+ 1 ct))
              (examinar-puntos (- num 1) 0 1)
              )
          )
      
      )
  )
; hace el llamado de la funcion que examina los puntos 5 por si hay mas de una linea llena.
(define (recursivo-examinar-puntos num ct )
  (if (not(= ct 6))
      (begin
        (examinar-puntos num 0 1)
        (recursivo-examinar-puntos num (+ ct 1))
        )
      )
  )
        

;#############################################################
;EVALUA QUE TECLA ES PARA EL MOVIMIENTO
  (define (evaluar-key);recibe un movimiento de teclado, devuelve un simbolo sirve para poder generar movimiento 
   (if(not(ready-key-press tetris))
        'down
        (if(and (equal? (key-value(get-key-press tetris)) 'right)(not(equal? (key-value(get-key-press tetris)) 'right)))
           (begin
       
            'right)
     (if(and (equal? (key-value(get-key-press tetris)) 'left)(not(equal? (key-value(get-key-press tetris)) 'left)))
        (begin
 
                'left)
              
             (evaluar-key)
                )
            
        )
    )
  )

;#########################################################;OJOOO
;LIMITES DE LA FIGURA4
(define(next-pos-fig4 y3 x2 x1)
  (if(not ( = 24 y3))
     (if  (and(= 0 (vector-ref (mascaravec-num (+ y3 1)) x1))(= 0 (vector-ref (mascaravec-num (+ y3 1)) x2)))
              #t
              #f
              )
     #f
     )
  )

(define (right-pos-fig4 y1 y2 y3 x1 x2)
  (if(not ( = 13 x1))
     (if (and(= 0 (vector-ref (mascaravec-num  y1 ) (+ 1 x1)))
             (= 0 (vector-ref (mascaravec-num  y2 ) (+ 1 x1)))
             (= 0 (vector-ref (mascaravec-num  y3 ) (+ 1 x2))))
              #t
              #f
              )
     #f
     )
  )

(define (left-pos-fig4 y1 y2 y3 x1 )
  (if(not ( = 0 x1))
     (if (and(= 0 (vector-ref (mascaravec-num  y1 ) (- x1 1)))
             (= 0 (vector-ref (mascaravec-num  y2 ) (- x1 1)))
             (= 0 (vector-ref (mascaravec-num  y3 ) (- x1 1))))
              #t
              #f
              )
     #f
     )
  )

 

;#########################################################
;figura4 es una L
(define(figuraL  y1 y2 y3 x1 x2 color)
  (begin
  (vector-set! (mascaravec-num y1) x1 color)
  (vector-set! (mascaravec-num y2) x1 color)
  (vector-set! (mascaravec-num y3) x1 color)
  (vector-set! (mascaravec-num y3) x2 color)
    (dibujar-rejilla 0 0 ))
  )


;#############################################################
;funcion que valida el movimiento de la figgura3 hcia abajo izq y derecha, redibuja la ventana si se llena una fila
;me hace hace el llamado de(recursivo-examinar-puntos 24 0) y me llama a la fig4 
(define(fig4 y1 y2 y3 x1 x2 key )
 (if (equal? key 'down)     
  (if (next-pos-fig4 y3 x2 x1)
     (begin 
       (figuraL  y1 y2 y3 x1 x2 1)
       (figuraL  y1 y2 y3 x1 x2 0)
       (figuraL  (+ 1 y1) (+ 1 y2) (+ 1 y3) x1 x2 1)
 
       (fig4(+ 1 y1) (+ 1 y2) (+ 1 y3) x1 x2 (evaluar-key) )
      )
      
  (begin
        (recursivo-examinar-puntos 24 0);VALIDA QUE EXISTA UNA FILA LLENA Y LA BORRA
      (caida-fig1 6 7 0 1))
      )
   (if(equal? key 'right)
        (if(right-pos-fig4 y1 y2 y3 x1 x2)
           (begin
             (figuraL  y1 y2 y3 x1 x2 1)
             (figuraL  y1 y2 y3 x1 x2 0)
             (figuraL  y1 y2 y3 (+ x1 1) (+ x2 1) 1)
             
             (fig4 y1 y2 y3 (+ x1 1) (+ x2 1) (evaluar-key) )
      )
           
   (fig4 y1 y2 y3 x1 x2(evaluar-key) )
           )
     (if(equal? key 'left)
        (if(left-pos-fig4 y1 y2 y3 x1 )
           (begin
               (figuraL  y1 y2 y3 x1 x2 1)
             (figuraL  y1 y2 y3 x1 x2 0)
             (figuraL  y1 y2 y3 (- x1 1) (- x2 1) 1)
     (fig4 y1 y2 y3 (- x1 1) (- x2 1) (evaluar-key) )
      )
     (fig4 y1 y2 y3 x1 x2(evaluar-key) )
    
           )

  )
     
  )
  )
  )


;#########################################################
;LIMITES DE LA FIGURA3
(define(next-pos-fig3 y4 x1)
  (if(not ( = 24 y4))
     (if  (= 0 (vector-ref (mascaravec-num (+ y4 1)) x1))
              #t
              #f
              )
     #f
     )
  )

(define (right-pos-fig3 y1 y2 y3 y4 x1)
  (if(not ( = 13 x1))
     (if (and(= 0 (vector-ref (mascaravec-num  y1 ) (+ 1 x1)))
             (= 0 (vector-ref (mascaravec-num  y2 ) (+ 1 x1)))
             (= 0 (vector-ref (mascaravec-num  y3 ) (+ 1 x1)))
             (= 0 (vector-ref (mascaravec-num  y4 ) (+ 1 x1))))
              #t
              #f
              )
     #f
     )
  )

(define (left-pos-fig3 y1 y2 y3 y4 x1)
  (if(not ( = 0 x1))
     (if (and(= 0 (vector-ref (mascaravec-num  y1 ) (- x1 1)))
             (= 0 (vector-ref (mascaravec-num  y2 ) (- x1 1)))
             (= 0 (vector-ref (mascaravec-num  y3 ) (- x1 1)))
             (= 0 (vector-ref (mascaravec-num  y4 ) (- x1 1))))
              #t
              #f
              )
     #f
     )
  )



 

;#########################################################
;figura3
(define(linea-de-4-vert  y1 y2 y3 y4 x1 color)
  (begin 
  (vector-set! (mascaravec-num y1) x1 color)
  (vector-set! (mascaravec-num y2) x1 color)
  (vector-set! (mascaravec-num y3) x1 color)
  (vector-set! (mascaravec-num y4) x1 color)
    (dibujar-rejilla 0 0 ))
  )
;#############################################################
;funcion que valida el movimiento de la figgura3 hcia abajo izq y derecha, redibuja la ventana si se llena una fila
;me hace hace el llamado de(recursivo-examinar-puntos 24 0) y me llama a la fig4 
(define(fig3 y1 y2 y3 y4 x1 key )
 (if (equal? key 'down)     
  (if (next-pos-fig3 y4 x1)
     (begin 
       (linea-de-4-vert  y1 y2 y3 y4 x1 1)
       (linea-de-4-vert  y1 y2 y3 y4 x1 0)
       (linea-de-4-vert  (+ 1 y1) (+ 1 y2) (+ 1 y3) (+ 1 y4) x1 1)
      (fig3 (+ 1 y1) (+ 1 y2) (+ 1 y3) (+ 1 y4) x1 (evaluar-key) )
    
      )
      
  (begin
        (recursivo-examinar-puntos 24 0);VALIDA QUE EXISTA UNA FILA LLENA Y LA BORRA
  (caida-fig4 0 1 2 5 6 ))
      )
   (if(equal? key 'right)
        (if(right-pos-fig3 y1 y2 y3 y4 x1)
           (begin
              (linea-de-4-vert  y1 y2 y3 y4 x1 1)
               (linea-de-4-vert  y1 y2 y3 y4 x1 0)
                (linea-de-4-vert  y1 y2 y3 y4 (+ x1 1) 1)
          
             
      (fig3 y1 y2 y3 y4 (+ x1 1) (evaluar-key) )
      )
      
      
      (fig3 y1 y2 y3 y4 x1 (evaluar-key) )
    
           )
     (if(equal? key 'left)
        (if(left-pos-fig3 y1 y2 y3 y4 x1)
           (begin
              (linea-de-4-vert  y1 y2 y3 y4 x1 1)
               (linea-de-4-vert  y1 y2 y3 y4 x1 0)
                (linea-de-4-vert  y1 y2 y3 y4 (- x1 1) 1)
          
             
      (fig3 y1 y2 y3 y4 (- x1 1) (evaluar-key) )
      )
      
      
      (fig3 y1 y2 y3 y4 x1 (evaluar-key) )
    
           )

  )
     
  )
  )
  )


;#########################################################
;LIMITES DE LA FIGURA 2
(define(next-pos-fig2 y1 x1 x2 x3 x4)
  (if(not ( = 24 y1))
     (if (and (= 0 (vector-ref (mascaravec-num (+ y1 1)) x1))(= 0 (vector-ref (mascaravec-num (+ y1 1)) x2))(= 0 (vector-ref (mascaravec-num (+ y1 1)) x3))(= 0 (vector-ref (mascaravec-num (+ y1 1)) x4)))
              #t
              #f
              )
     #f
     )
  )

(define (right-pos-fig2 y1 x4)
  (if(not ( = 13 x4))
     (if (= 0 (vector-ref (mascaravec-num  y1 ) (+ 1 x4)))
              #t
              #f
              )
     #f
     )
  )

(define (left-pos-fig2 y1 x1)
  (if(not ( = 0 x1))
     (if (= 0 (vector-ref (mascaravec-num  y1 ) (- x1 1)))
              #t
              #f
              )
     #f
     )
  )


;#########################################################
;figura 2
(define(linea-de-4  y1 x1 x2 x3 x4 color)
  (begin 
  (vector-set! (mascaravec-num y1) x1 color)
  (vector-set! (mascaravec-num y1) x2 color)
  (vector-set! (mascaravec-num y1) x3 color)
  (vector-set! (mascaravec-num y1) x4 color)
    (dibujar-rejilla 0 0 ))
  )
;#############################################################
;funcion que valida el movimiento de la figgura2 hcia abajo izq y derecha, redibuja la ventana si se llena una fila
;me hace hace el llamado de(recursivo-examinar-puntos 24 0) y me llama a la fig3 
(define(fig2 y1 x1 x2 x3 x4 key )
 (if (equal? key 'down)     
  (if (next-pos-fig2 y1 x1 x2 x3 x4)
     (begin (linea-de-4  y1 x1 x2 x3 x4 1)
      (linea-de-4  y1 x1 x2 x3 x4 0)
      (linea-de-4  (+ 1 y1) x1 x2 x3 x4 1)
      (fig2 (+ 1 y1) x1 x2 x3 x4  (evaluar-key) )
      )
      
  (begin
        (recursivo-examinar-puntos 24 0);VALIDA QUE EXISTA UNA FILA LLENA Y LA BORRA
     (caida-fig3 0 1 2 3 7))
      )
   (if(equal? key 'right)
        (if(right-pos-fig2 y1 x4)
           (begin (linea-de-4  y1 x1 x2 x3 x4 1)
      (linea-de-4  y1 x1 x2 x3 x4 0)
      (linea-de-4  y1 (+ x1 1) (+ x2 1) (+ x3 1) (+ x4 1) 1)
      (fig2 y1 (+ x1 1) (+ x2 1) (+ x3 1) (+ x4 1)  (evaluar-key) )
      )
      
      
      (fig2 y1 x1 x2 x3 x4(evaluar-key))
    
           )
     (if(equal? key 'left)
        (if(left-pos-fig2 y1 x1)
           (begin (linea-de-4  y1 x1 x2 x3 x4 1)
      (linea-de-4  y1 x1 x2 x3 x4 0)
      (linea-de-4  y1 (- x1 1) (- x2 1) (- x3 1) (- x4 1) 1)
      (fig2 y1 (- x1 1) (- x2 1) (- x3 1) (- x4 1)  (evaluar-key) )
      )
      
      (fig2 y1 x1 x2 x3 x4(evaluar-key))
    
           )

  )
     
  )
  )
  )

;#######################################################
;limites a nivel horizontal y a nivel vertical cuando cae la figura1 
(define (next-pos-fig1 x1 x2 y1 y2)
  (if(not ( = 24 y2))
     (if (and (= 0 (vector-ref (mascaravec-num (+ y2 1)) x1))(= 0 (vector-ref (mascaravec-num (+ y2 1)) x2)))
              #t
              #f
              )
     #f
     )
  )
(define (right-pos-fig1 x2  y1 y2)
  (if(not ( = 13 x2))
     (if (and (= 0 (vector-ref (mascaravec-num  y2 ) (+ 1 x2)))(= 0 (vector-ref (mascaravec-num y1) (+ 1 x2))))
              #t
              #f
              )
     #f
     )
  )
(define (left-pos-fig1 x1  y1 y2)
  (if(not ( = 0 x1))
     (if (and (= 0 (vector-ref (mascaravec-num  y2 ) (- x1 1)))(= 0 (vector-ref (mascaravec-num y1)  (- x1 1))))
              #t
              #f
              )
     #f
     )
  )
         
;#############################################################
;figura1
;me dibuja un cuadrado que corresponde a la figura 1
(define(cuadrado x1 x2 y1 y2 color)
  (begin 
   (vector-set! (mascaravec-num y1) x1 color)
      (vector-set! (mascaravec-num y1) x2 color)
      (vector-set! (mascaravec-num y2) x1 color)
      (vector-set! (mascaravec-num y2) x2 color)
  (dibujar-rejilla 0 0 ))
  )

;#############################################################
;dibuja la figura recursivamente y muestra el efecto de caida teniendo en cuenta que controla
;el movimiento hacia los lados
(define (figura1 x1 x2 y1 y2 key)
  (if (equal? key 'down)         ;si se oprime las flechas de izquierda y derecha
  (if (next-pos-fig1 x1 x2 y1 y2)
      
      (begin
        
        (cuadrado x1 x2 y1 y2 1)
        (cuadrado x1 x2 y1 y2 0)
        (cuadrado x1 x2 (+ y1 1) (+ y2 1) 1)
      
      (figura1 x1 x2 (+ 1 y1)(+ 1 y2)  (evaluar-key))
      )
      (begin
        (recursivo-examinar-puntos 24 0);VALIDA QUE EXISTA UNA FILA LLENA Y LA BORRA
     (caida-fig2 0 4 5 6 7 )
      )
      )
  (if(equal? key 'right)
    (if (right-pos-fig1 x2  y1 y2)
  (begin
    (cuadrado x1 x2 y1 y2 1)
        (cuadrado   x1  x2  y1 y2 0)
        (cuadrado (+ 1 x1) (+ x2 1) y1 y2 1)
      
      (figura1 (+ 1 x1) (+ x2 1) y1 y2 (evaluar-key))
    
           )
  (figura1 x1 x2 y1 y2  (evaluar-key))
  )
  
  (if(equal? key 'left)
     (if (left-pos-fig1 x1  y1 y2)
     (begin
       (cuadrado x1 x2 y1 y2 1)
        (cuadrado x1 x2 y1 y2 0)
        (cuadrado (- x1 1) (- x2 1) y1 y2 1)
      
      (figura1 (- x1 1) (- x2 1) y1 y2  (evaluar-key))
  
           )
     (figura1 x1 x2 y1 y2  (evaluar-key))
  )
)
      )
  )
  )
  
  

;valida que la persona pueda seguir jugando y llama a la funcion figura1 que me controla el efecto de caida de la figura 1
(define (caida-fig1  x1 x2 y1 y2)
  (if (not(= (vector-ref (mascaravec-num y2) x1) 1))
    (figura1 x1 x2 y1 y2 (evaluar-key) )
    
 (close-viewport tetris)
  ))

;valida que la persona pueda seguir jugando y llama a la funcion fig2 que me controla el efecto de caida de la figura 2
(define (caida-fig2  y1 x1 x2 x3 x4)
  (if (not(= (vector-ref (mascaravec-num y1) x1) 1))
    (fig2 y1 x1 x2 x3 x4 (evaluar-key))
    
 (close-viewport tetris)
  ))
;valida que la persona pueda seguir jugando y llama a la funcion fig2 que me controla el efecto de caida de la figura 2
(define (caida-fig3 y1 y2 y3 y4 x1)
  (if (not(= (vector-ref (mascaravec-num y4) x1) 1))
     
    (fig3 y1 y2 y3 y4 x1 (evaluar-key) )
     (begin
        (linea-de-4-vert  y1 y2 y3 y4 x1 1)
 (close-viewport tetris)
 )
  ))
;valida que la persona pueda seguir jugando y llama a la funcion fig2 que me controla el efecto de caida de la figura 2
(define (caida-fig4  y1 y2 y3 x1 x2)
  (if (and(not(= (vector-ref (mascaravec-num y3) x1) 1))(not(= (vector-ref (mascaravec-num y3) x2) 1))(not(= (vector-ref (mascaravec-num y2) x1) 1))
      (not(= (vector-ref (mascaravec-num y1) x1) 1)))
    (fig4  y1 y2 y3 x1 x2 (evaluar-key))
     (begin
    (figuraL  y1 y2 y3 x1 x2 1)
 (close-viewport tetris))
  ))

;MUESTRA UNA IMAGEN Y CARGA UNA BARRA PARA EMPEZAR A JUGAR.
(define TETRIS1 (open-viewport "TETRIS" 300 300 ))
(((draw-pixmap-posn "tet1.bmp" 'bmp) TETRIS1) (make-posn 0 0) "black")
(define (loader n x y)
  (if (>= n 0)
      (begin
        ((draw-solid-rectangle TETRIS1) (make-posn x y) 20 15 (make-rgb 0 0 0))
        (sleep 0.1)
        (loader (- n 1) (+ x 10) y)
        )
      ((draw-solid-rectangle TETRIS1) (make-posn x y) 15 15 (make-rgb 0 0 0))
      )
  )
(define (inicio)
  (begin
    (loader 18 50 260)
    (sleep 0.03)
    (close-viewport TETRIS1)
   (caida-fig1 6 7 0 1 )
    )
  )
(inicio)