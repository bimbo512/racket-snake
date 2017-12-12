#lang racket
;;Snake
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;PROYECTO FINAL FUNDAMENTOS DE PROGRAMACIÓN
;DOCENTE: Andres Mauricio Castillo 
;INTERGRANTES
;--Alvarado Juan Felipe
;--Cortés David Santiago
;--Hurtado Jose Alejandro
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require htdp/gui)
(require (prefix-in racket: racket/gui))
(require racket/string)

;;::::::::::::::::::DEFINICION DE ESTRUCTURAS::::::::::::::::::::::::::::::::::::
;world es una estructura,representa el estado del mundo, compuesta por otras estructuras; snake y fruta
(define-struct world (snake fruta bonus score) #:transparent)
;fruta es una estructura. Pos representa la celda que esta ocupando dentro del canvas posn(x,y)
(define-struct fruta (posn) #:transparent)
;bonus es una estructura. Es la fruta bonus del juego, contiene una posición dentro del canvas (posn)
;y un tiempo en pantalla expresado en segundos (t)
(define-struct bonus (posn t) #:transparent)
;score es una estructura unitaria, n es un número y representa el puntaje del jugador
(define-struct score (n) #:transparent)
;snake es una estructura. dir es un string, representa la direccion de un segmento del snake.
;pos es la celda que ocupa en coordenadas posn(x,y)
(define-struct snake (segs dir) #:transparent)
;posn es una estructura que representa las coordenadas dentro del canvas (x, y)
(define-struct posn (x y) #:transparent)

;::::::::::::::::::::::::DEFINICION DE CONSTANTES:::::::::::::::::::::::::::::::::::

(define CELDA 15)
(define N-FILAS 35)
(define N-COLUMNAS 35)
(define TICK 0.07)
(define EXP 4)

(define ANCHO (* N-COLUMNAS CELDA))
(define LARGO (* N-FILAS CELDA))

(define FONDO (empty-scene ANCHO LARGO "black"))

(define BODY (rectangle (/ CELDA 1.5) (/ CELDA 1.5) "solid" "white" ))

(define TEST
  (underlay
   (rectangle (/ CELDA 1.5) (/ CELDA 1.5) "solid" "gold")
   (circle (/ (/ CELDA 1.5) 3) "solid" "black")))

(define APPLE (rectangle (/ CELDA 1.5) (/ CELDA 1.5) "solid" "green"))
(define BONO (rectangle (/ CELDA 1.5) (/ CELDA 1.5) "solid" "yellow"))

;Contrato: tiempo-bonus: world->number
;Proposito: Funcion que dice el tiempo del bono
(define (tiempo-bonus w)
  (bonus-t (world-bonus w)))
;Contrato: loc-bonus: world->punto2d
;Proposito: Funcion que dice donde se encuentra el punto del bono
(define (loc-bonus w)
  (bonus-posn (world-bonus w)))
;Contrato: puntaje: world->number
;Proposito: Funcion que dice el puntaje del jugador
(define (puntaje w)
  (calc-score (snake-segs (world-snake w)) 0))
;Constantes del mundo
(define WORLD0
  (make-world (make-snake (list (make-posn 2 6) ) "right")
              (make-posn 1 15) (make-bonus (make-posn 1 10) EXP)(make-score 0)))
  
;::::::::::::::::::::::::::::::::::::::::VARIABLES DE TESTEO:::::::::::::::::::::::::::::::::::::::::::::::::::
(define food1 (make-posn 2 5))
(define segs1 (list (make-posn 2 6))) ; one-segment snake
(define segs2 (list (make-posn 2 5) (make-posn 3 5)))
(define segs3 (list (make-posn 2 5) (make-posn 3 5) (make-posn 3 1)))
(define snake1 (make-snake segs1 'up))
(define snake2 (make-snake segs2 'up))
(define snake3 (make-snake segs3 'up))
;(define world1 (make-world snake1 food1))
;(define world2 (make-world snake2 food1 10)) ; eating

;:::::::::::::::::::::::::::::::::::::::::FUNCIONES DEL MUNDO::::::::::::::::::::::::::::::::::::::::::::::::::::
;;FUNCIONES PARA RENDERIZAR

;Contrato: render: world -> image
;Propósito: Renderizar el estado del mundo
(define (render w)
  (cond
    [(>= (tiempo-bonus w) 0)
     (place-image
      (name+img)
      30 15
      
     (place-image
                              (fig-score (snake-segs (world-snake w)) w)
                              480 15
                              (snake+img (world-snake w)
                                         (food+img (world-fruta w)
                                                   (bono+img w
                                                             FONDO)))))]
    [else
     (place-image
      (name+img)
      30 15
      
     (place-image
      (fig-score (snake-segs (world-snake w)) w)
      480 15
      (snake+img (world-snake w)
                 (food+img (world-fruta w)
                           FONDO))))]))

;Contrato: imagen-en-celda: imagen numero numero imagen -> imagen
;Proposito:  dibuja imagen1 en el centro de una celda (x,y) dada en la imagen2
(define (imagen-en-celda img1 celda-x celda-y img2)
  (place-image
   img1
   (* CELDA (+ celda-x 0.5))
   (* CELDA (- N-FILAS (+ celda-y 0.5)))
   img2))

;Contrato:snake+img: snake image -> image. Donde snake es una estructura
;Propósito: Dibujar el snake en el canvas
;Ejemplo: (snake+img snake1 FONDO) debe retornar la imagen del fondo con el snake pintado en ella
(define (snake+img snake img)
  (segs+img (snake-segs snake) img))

;Contrato: segs+img: list image -> image
;Proposito: Funcion que dibuja todos los segmentos
;Ejemplo:(segs+img segs2 FONDO) debe retornar la imagen del fondo con 2 segmentos del snake pintados en él
(define (segs+img loseg img)
  (cond
    [(empty? loseg) img]
    [else
     (imagen-en-celda
      BODY
      (posn-x (first loseg)) (posn-y (first loseg))
      (segs+img (rest loseg) img))]))
  
;Contrato:food+img: fruta image -> image
;Propósito: Funcion que dibuja la fruta en el canvas
;Ejemplo: (food+img food1 FONDO) Debe retornar la imagen del fondo con la fruta pintada en él
(define (food+img fruta img)
  (imagen-en-celda APPLE (posn-x fruta) (posn-y fruta) img))
;Contrato:food+img: bonus image -> image
;Propósito: Funcion que dibuja el bono en el canvas
;Ejemplo: (food+img WORLD0 FONDO) Debe retornar la imagen del fondo con el bono pintado en él
(define (bono+img w img)
  (imagen-en-celda BONO (posn-x (loc-bonus w)) (posn-y (loc-bonus w)) img))

;Contrato: score+img: number -> image
;Propósito: Hacer que el puntaje aparezca durante el juego y se actualice
;(define (score+img score img)
 ; (imagen-en-celda (fig-score (snake-segs score)) 510 15 img))
(define (name+img)
  (text (text-contents nombre) 20 "cyan"))

;Contrato: fig-score: list world->string
;Proposito: Funcion que pinta el score en el mundo
;Ejemplo: (fig-score segs1 WORLD0) debe retornar .
(define (fig-score x w)
  (cond
    [(comiendo-bonus? w) (text (string-append "Score: " (number->string (+ (calc-score x 0) 2))) 20 "white")]
    [else
     (text (string-append "Score: " (number->string (calc-score x 0))) 20 "white")]))

;Contrato: calc-score: list number->number
;Proposito: Funcion que calcula el puntaje de acuerdo al numero de segmentos -1
;Ejemplo: (calc-score segs1 2) debe retornar 2
(define (calc-score serpiente n)
  (cond
    [(empty? serpiente) n]
    [(<= (length serpiente) 1) n]
    [else (calc-score (rest serpiente) (+ n 1))]))
  
;Contrato: last-scene: world->image
;Proposito: Funcion que dibuja la última escena
(define (last-scene w)
  (ripsnek w)
  (place-image
   (above
    (text/font "HAS MUERTO" 30 "red" "Times New Roman" 'default 'normal 'bold #f)
    (fig-score (snake-segs (world-snake w)) w))
   (/ ANCHO 2) (/ LARGO 2)
   (render w)))
;;____________________________FUNCIONES PARA EL MOVIMIENTO______________________________
;Contrato: snake-grow: snake -> snake. Donde snake es una estructura
;Proposito: Funcion que añade un nuevo segmento a la serpiente en la "cabecera" a una direccion dada
(define (snake-grow snake)
  (make-snake (cons (new-seg (first (snake-segs snake)) (snake-dir snake))
                    (snake-segs snake))
              (snake-dir snake)))

;Contrato: new-seg: snake-dir -> snake-seg
;Proposito: Funcion que crea un nuevo segmento
(define (new-seg seg dir)
  (cond
    [(string=? "up" dir) (make-posn (posn-x seg) (+ (posn-y seg) 1))]
    [(string=? "down" dir) (make-posn (posn-x seg) (- (posn-y seg) 1))]
    [(string=? "left" dir) (make-posn (- (posn-x seg) 1) (posn-y seg))]
    [else (make-posn (+ (posn-x seg) 1) (posn-y seg))]))

;Contrato: snake-slither: snake -> snake. Donde snake es una estructura
;Proposito: Funcion que simula el movimiento de la serpiente eliminando el ultimo segmento
;y añadiendo otro al inicio, teniendo como referencia la direccion en la
;que se esta  moviendo
(define (snake-slither snake)
  (make-snake (cons (new-seg (first (snake-segs snake)) (snake-dir snake))
                    (nuke-last (snake-segs snake)))
              (snake-dir snake)))

;Contrato: nuke-last: snake-> snake
;Proposito: Funcion que retorna una snake sin su ultimo segmento
(define (nuke-last loseg)
  (cond
    [(empty? (rest loseg)) empty]
    [else
     (cons (first loseg) (nuke-last (rest loseg)))]))
;_____________________________________COLISIONES____________________________________
;Contrato: comiendo?: world -> boolean. Donde w es una estructura
;Proposito: Funcion que determina si la cabecera de la serpiente colisiona con una fruta
;Ejemplo: (comiendo? WORLD0) debe retornar #f
(define (comiendo? w)
  (posn=? (first (snake-segs (world-snake w))) (world-fruta w)))
;Contrato: comiendo-bonus?: world -> boolean. Donde w es una estructura
;Proposito: Funcion que determina si la cabecera de la serpiente colisiona con una fruta
;Ejemplo: (comiendo-bonus? WORLD0) debe retornar #f
(define (comiendo-bonus? w)
  (posn=? (first (snake-segs (world-snake w))) (loc-bonus w)))

;Contrato: posn=?: posn posn -> boolean. Donde a y b son puntos 2d
;Proposito: Funcion que determina si dos puntos estan sobrelapados
(define (posn=? a b)
  (and (= (posn-x a ) (posn-x b)) (= (posn-y a) (posn-y b))))

;Contrato: self-colission?: world -> boolean. Donde w es una estructura
;Proposito: Funcion que determina si la serpiente se esta chocando con sigo misma
(define (self-collision? w)
  (seg-collision? (first (snake-segs (world-snake w))) (rest (snake-segs (world-snake w)))))

;Contrato: seg-colission?: segs -> boolean
;Proposito: Funcion que determina si un segmento dado esta en el mismo lugar que algún otro en la lista
(define (seg-collision? seg los)
  (cond
    [(empty? los) false]
    [else
     (or (posn=? seg (first los)) (seg-collision? seg (rest los)))]))

;Contrato: world-collision?: world -> boolean. donde w es una estructura
;Proposito: Funcion que determina si la serpiente esta chocando con uno de los bordes del mundo
(define (world-collision? w)
  (not (in-bounds? (first (snake-segs (world-snake w))))))

;Contrato: in-bounds?: p -> boolean. Donde p es un punto 2d
;Proposito: Funcion que determina si un determinado punto esta en el borde del canvas
(define (in-bounds? p)
  (and (>= (posn-x p) 0) (< (posn-x p) N-COLUMNAS)
       (>= (posn-y p) 0) (< (posn-y p) N-FILAS)))
;Contrato: muerto?: world->boolean
;Porposito: Funcion que determina si la serpiente ha muerto
(define (muerto? w)
  (or (world-collision? w) (self-collision? w)))

;Contrato: end?: world->boolean
;Porposito: Funcion que evalúa si el jugador ha perdido. es decir, si choca con un muro o si choca consigo mismo
;Ejemplo: (end? WORLD0) debe retornar #f
(define (end? w)
  (if (muerto? w)
  (cond
    [(eqv? (crear-txt w) "puntajes.txt") true]
    [else false]) false))
       
;:::::::::::::::::::::::::::::::::::FUNCIONES LOGICAS:::::::::::::::::::::::::::::::::::::::::::
;Contrato: zerobonus?: world->boolean-number
;Proposito: Funcion que determina si el bonus expiró
;Ejemplo: (zerobonus? WORLD0) debe retornar 4
(define (zerobonus? w)
  (cond
    [(zero? (tiempo-bonus w)) true]
    [(<= (tiempo-bonus w) 4) 4]
    [else false]
    ))
;Contrato: resetbonus: world->boolean
;Proposito: Funcion que reaparece el bonus
(define (resetbonus w)
  (<= (tiempo-bonus w) (* EXP -1)))

;Contrato: next-world: world -> world. donde w es una estructura.
;Propósito: Funcion que calcula el nuevo estado del mundo cada tick del reloj
;Ejemplo: (next-world WORLD0) debe retornar (world (snake (list (posn 3 6)) "right") (posn 1 15) (bonus (posn 1 10) 4) (score 0))
(define (next-world w)
  (cond
    [(world-collision? w) WORLD0]
    [(self-collision? w) WORLD0]
    [(comiendo? w) (make-world
                    (snake-grow (world-snake w))
                    (make-posn (random N-COLUMNAS)
                               (random N-FILAS))
                    (make-bonus (loc-bonus w)
                                (cond
                                  [(resetbonus w) EXP]
                                  [else
                                   (sub1 (tiempo-bonus w))]))
                    (calc-score (snake-segs (world-snake w)) 0))]
;la serpiente es capaz de crecer dos veces porque se hace el llamado a snake-grow
;semi-recursivamente, evalua snake-grow si misma evaluada en el cuerpo de la serpiente
    [(comiendo-bonus? w) (make-world
                          (snake-grow (snake-grow (world-snake w)))
                          (make-posn (random N-COLUMNAS)
                                     (random N-FILAS))
                          (make-bonus (make-posn (random N-COLUMNAS)(random N-FILAS))
                                      -1)
                          (+ (calc-score (snake-segs (world-snake w)) 0) 2))]
    
    [else
     (make-world (snake-slither (world-snake w))
                 (world-fruta w) (world-bonus w) (world-score w))]))

;Contrato: tecla: world key-event -> world. Donde w es una estructura y kev
;Propósito: Funcion que determina el key-event para el movimiento de la serpiente con las teclas
;Ejemplo:(tecla WORLD0 "up") debe retornar (world (snake (list (posn 2 6)) "up") (posn 1 15) (bonus (posn 1 10) 4) (score 0))
(define (tecla w kev)
  (cond
    [(and (key=? kev "up") (string=? (snake-dir (world-snake w)) "down"))
     (make-world (make-snake (snake-segs (world-snake w)) "down") (world-fruta w) (world-bonus w) (world-score w))]
    
    [(and (key=? kev "down") (string=? (snake-dir (world-snake w)) "up"))
     (make-world (make-snake (snake-segs (world-snake w)) "up") (world-fruta w) (world-bonus w) (world-score w))]
    
    [(and (key=? kev "left") (string=? (snake-dir (world-snake w)) "right"))
     (make-world (make-snake (snake-segs (world-snake w)) "right") (world-fruta w) (world-bonus w) (world-score w))]
    
    [(and (key=? kev "right") (string=? (snake-dir (world-snake w)) "left"))
     (make-world (make-snake (snake-segs (world-snake w)) "left") (world-fruta w) (world-bonus w) (world-score w))]
    
    [else
     (make-world (make-snake (snake-segs (world-snake w)) kev) (world-fruta w) (world-bonus w) (world-score w))]))

;:::::::::::::::::::::::FUNCIONES DE PUNTAJE Y GUARDAR PUNTAJE:::::::::::::::::::::::::::::::::
;Funciones para el sonido de la muerte del snake
;Funciones auxiliares:
(define pth "C:/Users/Estudiante/Documents/Racket/snake-gam/Snake.wav")        ;Define el camino del archivo de sonido de la serpiente
(define play-asynchronously #t) ;Continua en ejecución mientras el sonido se reproduce

;Contrato: ripsnek : w-> Sonido (WAV)
;Propósito: Reproducir un sonido en caso de muerte de la serpiente
(define (ripsnek w)
  (cond
    [(or (world-collision? w) (self-collision? w)) (racket:play-sound pth play-asynchronously)]
    [else w]))

;crear-txt: escribe el nombre insertado y el puntaje alcanzado hasta morir
;en un archivo de texto contenido en el directorio del programa
;texto: valor, guarda en forma de string el contenido del archivo de texto

;Contrato: guarda-puntaje: world -> string
;Propósito: guardar dentro de un archivo de texto el puntaje del jugador y su nombre
;en todos los casos retorna el nombre del archuivo en el que se escribio en forma de string
;Ejemplo:
;(guarda-puntaje WORLD0) Debe retornar => "puntajes.txt"
(define texto (read-file "puntajes.txt"))
(define (crear-txt w)
       (write-file "puntajes.txt" (string-append (text-contents nombre) "," (number->string (puntaje w)))))

(define texto2 (read-file "puntajes.txt"))
;:::::::::::::::::::::::::::::::::::::VENTANAS::::::::::::::::::::::::::::::::
(define header (make-message "
      SNAKE ADVENTURE - FDP
" ))

(define instrucciones
  (make-message "Mover con las flechas de dirección del teclado.
Consigue el mayor número de puntos."))

(define nombre
  (make-text "Nombre:"))

(define (w1 e)
    (create-window
      (list
       (list nombre)
       (list (make-button "Jugar" main))))#t)

(define (w2 e)
  (create-window
  (list
   (list (make-message (string-append "Último puntaje"
                                      texto2)))))#t)

(define (w3 e)
  (create-window
   (list
    (list instrucciones)))#t)
;ventana principal
(define w
    (create-window
      (list
       (list header)
       (list (make-button "Jugar" w1))
       (list (make-button "Último Puntaje" w2))
       (list (make-button "Instrucciones" w3))
       (list (make-button "Salir" (lambda (e) (hide-window w)))))))
  
;;BIG-BANG  
(define (main w)
  (big-bang WORLD0
    [to-draw render]
    [on-tick next-world TICK]
    [on-key tecla]
    [stop-when end? last-scene]
[name "SNAKE ADVENTURE"]) #t)
