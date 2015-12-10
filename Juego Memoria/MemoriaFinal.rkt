#lang racket
;***************************************************************************
;Desarrollador Gabriel Gutierres Tamayo 
;ggutierreztamayo@utp.edu.co
;Proyecto Final de Programacion 1. UTP 2015-2.
;***************************************************************************
(require (lib "graphics.ss" "graphics"))(open-graphics)
;Se dibujan las ventanas
(define ventana (open-viewport "Memoria" 630 700))
(define fondo (open-pixmap "Memoria" 630 700))

;Se dibujan los fondos
((draw-pixmap fondo) "Imagenes/Fondo1.jpg" (make-posn 0 0))
((draw-pixmap fondo) "Imagenes/fondo_matriz.jpg" (make-posn 90 90))
((draw-rectangle fondo)(make-posn 90 90) 450 450 "black")

;Se dibujan las imagenes de texto
(((draw-pixmap-posn "Imagenes/Aciertos.gif" 'gif/mask) fondo)(make-posn 100 550))
(((draw-pixmap-posn "Imagenes/Errores.gif" 'gif/mask) fondo)(make-posn 350 550))
(((draw-pixmap-posn "Imagenes/Tiempo.gif" 'gif/mask) fondo)(make-posn 230 600))

;Se dibujan los 16 contenedores correspondientes a las imagenes en el pixmap fondo
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 100 100))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 210 100))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 320 100))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 430 100))

(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 100 210))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 210 210))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 320 210))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 430 210))

(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 100 320))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 210 320))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 320 320))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 430 320))

(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 100 430))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 210 430))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 320 430))
(((draw-pixmap-posn "Imagenes/Fondo_Contenedor.gif" 'gif/mask) fondo)(make-posn 430 430))

;Se definen los dos fondos de contenedores de imagenes
(define Contenedor "Imagenes/Contenedor.gif")
(define Fondo_Contenedor "Imagenes/Fondo_Contenedor.gif")

;Se crea el cuadro donde se ejecutara el cronometro
((draw-rectangle fondo)(make-posn 379 605) 70 20)
((draw-solid-rectangle fondo)(make-posn 380 606) 68 18 "white")
((draw-string fondo) (make-posn 385 620) "00" "blue")
((draw-string fondo) (make-posn 405 620) ":" "blue")

;Se crean los cuadros que contienen las puntuaciones de aciertos y errores
((draw-rectangle fondo)(make-posn 270 555) 40 20)
((draw-solid-rectangle fondo)(make-posn 271 556) 38 18 "white")
((draw-rectangle fondo)(make-posn 510 555) 40 20)
((draw-solid-rectangle fondo)(make-posn 510 556) 38 18 "white")

;Se copia la ventana del pixmap "Fondo" a la ventana que se visualiza
(copy-viewport fondo ventana)

;Se define el fondo azul del contenedor de imagen
(define (Fondo_Imagen posicion) 
  (((draw-pixmap-posn Contenedor 'gif/mask) ventana)posicion))

;Se define el fondo del contenedor que no visualizara la imagen
(define (Fondo_Imagen2 posicion) 
  (((draw-pixmap-posn Fondo_Contenedor 'gif/mask) ventana)posicion))

;se define cada imagen
(define imagen1 "Imagenes/imagen1.gif")
(define imagen2 "Imagenes/imagen2.gif")
(define imagen3 "Imagenes/imagen3.gif")
(define imagen4 "Imagenes/imagen4.gif")
(define imagen5 "Imagenes/imagen5.gif")
(define imagen6 "Imagenes/imagen6.gif")
(define imagen7 "Imagenes/imagen7.gif")
(define imagen8 "Imagenes/imagen8.gif")

;Se guardan las listas de imagenes y posiciones
;Cada imagen esta repetida dos veces
(define imagenes (list imagen1 imagen2 imagen3 imagen4 imagen5 imagen6 imagen7 imagen8
                       imagen1 imagen2 imagen3 imagen4 imagen5 imagen6 imagen7 imagen8))
(define posiciones (list (make-posn 100 100) (make-posn 210 100) (make-posn 320 100) (make-posn 430 100)
                         (make-posn 100 210) (make-posn 210 210) (make-posn 320 210) (make-posn 430 210)
                         (make-posn 100 320) (make-posn 210 320) (make-posn 320 320) (make-posn 430 320)
                         (make-posn 100 430) (make-posn 210 430) (make-posn 320 430) (make-posn 430 430)))

;Esta lista tiene asignado el estado de cada imagen
(define estado_imagen (make-vector 16 0))

;Se elimina el elemento de la k-esima posion de una lista
(define (sacar_de_lista lista k)
  (append (reverse (list-tail (reverse lista) (- (length lista) k))) (list-tail lista (+ k 1))))

;Vector con aleatorios 
(define aleatorio
  (vector (random 1) (random 2) (random 3) (random 4)
          (random 5) (random 6) (random 7) (random 8)
          (random 9) (random 10) (random 11) (random 12)
          (random 13) (random 14) (random 15) (random 16)))

;Se crea una lista con las imagenes cambiandolas de orden en forma aleatoria
(define (imagenes_aleatorias lista_imagenes)
  (cond
    [(not (empty? lista_imagenes)) (cons (list-ref lista_imagenes (vector-ref aleatorio (- (length lista_imagenes) 1))) (imagenes_aleatorias (sacar_de_lista lista_imagenes (vector-ref aleatorio (- (length lista_imagenes) 1)))))]
    [else null]))

;se define el tiempo inicial
(define tiempo_inicial (current-seconds))

;Funcion que cuenta segundos desde 0, n puede ser cualquier valor
(define (contador_segundos n)
  (- (current-seconds) tiempo_inicial))

;Funcion boleana que determina si el click se produjo dentro de las dimensiones de un contenedor de imagen
(define (click_contenedor? posx posy click)
  (if (and (> (posn-x (mouse-click-posn click)) posx)
           (< (posn-x (mouse-click-posn click)) (+ posx 100))
           (> (posn-y (mouse-click-posn click)) posy)
           (< (posn-y (mouse-click-posn click)) (+ posy 100)))
      #t
      #f))

;Determina si le da click a uno de los 16 contenedores
(define (click_contenedores? click n)
  (cond 
    [(< n 16) (cond
                [(click_contenedor? (posn-x (list-ref posiciones n)) (posn-y (list-ref posiciones n)) click) #t]
                [else (click_contenedores? click (+ n 1))])]
    [else #f]))

;Lanza la posicion del contenedor donde se produjo el click
(define (posicion_contenedor click n)
  (if (click_contenedores? click 0)
      (cond
        [(click_contenedor? (posn-x (list-ref posiciones n)) (posn-y (list-ref posiciones n)) click) (make-posn (posn-x (list-ref posiciones n)) (posn-y (list-ref posiciones n)))]
        [else (posicion_contenedor click (+ n 1))])
      (void)))

;Lanza la posicion dentro del vector del contenedor al que se le da click
(define (posicion_vector_contenedor click n)
  (if (click_contenedores? click 0)
      (cond
        [(click_contenedor? (posn-x (list-ref posiciones n)) (posn-y (list-ref posiciones n)) click) n]
        [else (posicion_vector_contenedor click (+ n 1))])
      (void)))

;Funcion que realiza el cambio de estado de una imagen dentro de un vector
(define (cambiar_estado_imagen k imagen)
  (begin (vector-set! estado_imagen k imagen) estado_imagen))

;Se crea funcion booleana que determina si n=1
(define (igual? n)
  (if (equal? n 1) #t #f))

;Determina el numero de click
;el parametro n puede ser cualquier valor
(define (numero_click n)
   (length (filter igual? (vector->list estado_imagen))))

;Determinan si es el primer o segundo click
(define (primer_click? n)
  (if (equal? (numero_click n) 1) #t #f))

(define (segundo_click? n)
  (if (equal? (numero_click n) 2) #t #f))

;Invierte un vector
(define (invertir_vector vect)
  (list->vector (reverse (vector->list vect))))

;Lanza la posicion dentro del vector en la que se dio el primer click
;En el valor de posicion_click2 se coloca la posicion dentro del vector del en la cual ocurrio el segundo click
; n tomara el valor de 0
(define (posicion_vector_click1? vect n)
  (cond
    [(<= n (vector-length vect)) (cond
                                   [(equal? (vector-ref vect n) 1) n]
                                   [else (posicion_vector_click1? vect (+ n 1))])]
    [else (void)]))

;2
(define (posicion_vector_click2? vect n)
  (cond
    [(<= n (vector-length vect)) (cond
                                   [(equal? (vector-ref vect n) 1) (- 15 n)]
                                   [else (posicion_vector_click2? vect (+ n 1))])]
    [else (void)]))

;Funcion que borra las imagenes despues de un segundo de haber hundido la segunda
(define (borrar_imagenes n posicion1 posicion2)
 (cond
   [(and (>= (- (current-milliseconds) n) 0) (<= (- (current-milliseconds) n) 400)) (borrar_imagenes n posicion1 posicion2)]
   [(and (>= (- (current-milliseconds) n) 401) (<= (- (current-milliseconds) n) 800)) (begin (Fondo_Imagen2 posicion1)
                                                                               (Fondo_Imagen2 posicion2)
                                                                               (borrar_imagenes n posicion1 posicion2))]
   [else (void)]))

;Determina si la imagen de click1 y click 2 son iguales
;Imagen1 e imagen2 corresponden a las posiciones dentro de la lista de imagenes aleatorias
(define (imagenes_iguales? imagen1 imagen2)
  (cond
    [(equal? (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click1? estado_imagen 0)) (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click2? (invertir_vector estado_imagen) 0))) #t]
    [else #f]))

;Funcion que borra el 1 las imagenes del vector de estado de imagenes
(define (estado_guardar_imagen n)
  (begin
    (vector-set! estado_imagen (posicion_vector_click1? estado_imagen 0) 2)
    (vector-set! estado_imagen (posicion_vector_click2? (invertir_vector estado_imagen) 0) 2)
    estado_imagen))

;Funcon que borra la imagen tanto visualmente como del vector de estado de imagenes
(define (estado_borrar_imagen n)
  (begin 
      (borrar_imagenes (current-milliseconds) (list-ref posiciones (posicion_vector_click1? estado_imagen 0)) (list-ref posiciones (posicion_vector_click2? (invertir_vector estado_imagen) 0)))
      (cambiar_estado_imagen (posicion_vector_click1? estado_imagen 0) 0)
      (cambiar_estado_imagen (posicion_vector_click2? (invertir_vector estado_imagen) 0) 0)
      estado_imagen))
;Si las imagenes son iguales el cambia el estado de la imagen por un 2, de lo contrario por un 0
(define (asignar_estado_imagen n)
  (cond 
    [(imagenes_iguales? (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click1? estado_imagen 0)) (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click2? (invertir_vector estado_imagen) 0))) 
           (estado_guardar_imagen 0)]
    [else (estado_borrar_imagen 0)]))

;Se definen los aciertos y los errores en un vector, el primer elemento es aciertos, el segundo es de errores
(define aciertos_errores
  (vector 0 0))

;Funcion que incrementa el vector de aciertos y errores
(define (incrementar_puntos n)
  (cond
    [(imagenes_iguales? (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click1? estado_imagen 0)) (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click2? (invertir_vector estado_imagen) 0))) 
     (begin (vector-set! aciertos_errores 0 (+ (vector-ref aciertos_errores 0) 1)) aciertos_errores)]
    [else (begin (vector-set! aciertos_errores 1 (+ (vector-ref aciertos_errores 1) 1)) aciertos_errores)]))

;Funcion que visualiza los puntos de aciertos y errores despues del segundo click
(define (visualizar_puntos n)
  (cond
    [(imagenes_iguales? (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click1? estado_imagen 0)) (list-ref (imagenes_aleatorias imagenes) (posicion_vector_click2? (invertir_vector estado_imagen) 0))) 
     (begin ((clear-string ventana) (make-posn 280 570) (number->string (vector-ref aciertos_errores 0)))
            ((draw-string ventana)(make-posn 280 570) (number->string (vector-ref (incrementar_puntos 0) 0))))]
    [else (begin ((clear-string ventana) (make-posn 520 570) (number->string (vector-ref aciertos_errores 1)))
            ((draw-string ventana)(make-posn 520 570) (number->string (vector-ref (incrementar_puntos 0) 1))))]))

;Descuenta puntuacion de acuerdo a la cantidad de segundos jugados, p tomara cualquier valor
(define (menospuntos_tiempo p)
  (cond
    [(> (contador_segundos 0) 30) (* (- (contador_segundos 0) 30) 100)]
    [(> (contador_segundos 0) 60) (+ 3000 (* (- (contador_segundos 0) 30) 50))]
    [(>= (contador_segundos 0) 120) 8000]
    [else 0]))

;Descuenta puntuacion de acuerdo a la cantidad de errores, p tomara cualquier valor
(define (menospuntos_errores p)
  (* (vector-ref aciertos_errores 1) 200))

;Adiciona puntos de acuerdo la cantidad de aciertos, p tomara cualquier valor
(define (maspuntos_aciertos p)
  (cond
    [(< (contador_segundos 0) 40) 0]
    [else (* (vector-ref aciertos_errores 0) 110)]))

;Puntuacion total, p= 10000
(define (puntuacion_total p)
  (cond
    [(and (>= (contador_segundos 0) 120) (> (vector-ref aciertos_errores 1) (vector-ref aciertos_errores 0))) (* (vector-ref aciertos_errores 0) 90)]
    [else (- (+ p (maspuntos_aciertos 0)) (+ (menospuntos_errores 0) (menospuntos_tiempo 0)))]))


;Funcion que detecta si el click fue en un contenedor y en cual de los 16, si es primer click o segundo click
(define (mostrar_imagen click)
  (if (click_contenedores? click 0)
      (begin
        (cambiar_estado_imagen (posicion_vector_contenedor click 0) 1)
        (cond
          [(primer_click? 0) (begin
                               (Fondo_Imagen (posicion_contenedor click 0))
                               (((draw-pixmap-posn (list-ref (imagenes_aleatorias imagenes) (posicion_vector_contenedor click 0)) 'gif/mask) ventana)(posicion_contenedor click 0)))]
          [(segundo_click? 0) (begin 
                                (Fondo_Imagen (posicion_contenedor click 0))
                                (((draw-pixmap-posn (list-ref (imagenes_aleatorias imagenes) (posicion_vector_contenedor click 0)) 'gif/mask) ventana)(posicion_contenedor click 0))
                                (visualizar_puntos 0)
                                (asignar_estado_imagen 0))]
          [else (void)]))
      (void)))
    
;Se crea el cronometro, el cual solo contara hasta 2 minutos tiempo maximo que tendra el jugador
(define (juego n s minutos click)
 (cond
   [(equal? (vector-ref aciertos_errores 0) 8) (begin
                                                 (((draw-pixmap-posn "Imagenes/Puntuacion_Total.gif" 'gif/mask) ventana)(make-posn 165 660))
                                                 ((draw-string ventana) (make-posn 480 680) (number->string (puntuacion_total 10000))))]
   [(>= minutos 2) (begin
                     ((draw-string ventana) (make-posn 415 620) "00" "blue")
                     (((draw-pixmap-posn "Imagenes/Puntuacion_Total.gif" 'gif/mask) ventana)(make-posn 165 660))
                     ((draw-string ventana) (make-posn 480 620) (number->string (puntuacion_total 10000)) "blue"))]
   [(not (equal? click #f)) (begin
                              (mostrar_imagen (get-mouse-click ventana))
                              (juego n s minutos (ready-mouse-release ventana)))]
   [(<= (- (current-seconds) s) 9) (cond
                                     [(equal? n (current-seconds)) (begin
                                                                     ((draw-string ventana) (make-posn 415 620) "0" "blue")
                                                                     ((draw-string ventana) (make-posn 425 620) (number->string (- (current-seconds) s)) "blue")
                                                                     (juego n s minutos (ready-mouse-release ventana)))]
                                     [(not (equal? n (current-seconds)))
                                      (begin ((clear-solid-rectangle ventana)(make-posn 425 606) 9 18)
                                             (juego (current-seconds) s minutos (ready-mouse-release ventana)))])]
   [(>= (- (current-seconds) s) 60) (begin 
                                      ((clear-solid-rectangle ventana)(make-posn 392 606) 9 18)
                                      ((clear-solid-rectangle ventana)(make-posn 415 606) 18 18)
                                      ((draw-string ventana) (make-posn 395 620) (number->string (+ minutos 1)) "blue")
                                      (juego n (+ s 60) (+ minutos 1) (ready-mouse-release ventana)))]
   [(equal? n (current-seconds)) (begin ((draw-string ventana) (make-posn 415 620) (number->string (- (current-seconds) s)) "blue")
                                        (juego n s minutos (ready-mouse-release ventana)))]
   [(not (equal? n (current-seconds)))
   (begin ((clear-solid-rectangle ventana)(make-posn 415 606) 27 18)
          (juego (current-seconds) s minutos click))]
   [else (juego (current-seconds) s minutos click)]))
(juego tiempo_inicial tiempo_inicial 0 (ready-mouse-release ventana))