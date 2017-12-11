#lang racket
;;Snake
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require htdp/gui)

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

(define (tiempo-bonus w)
  (bonus-t (world-bonus w)))

(define (loc-bonus w)
  (bonus-posn (world-bonus w)))

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

;Contrato:snake+img: snake image -> image.
;Propósito: Funcion que dibuja el snake en el canvas. Donde snake es una estructura 
;Ejemplo:
(define (snake+img snake img)
  (segs+img (snake-segs snake) img))

;Contrato: segs+img: list image -> image
;Proposito: Funcion que dibuja todos los segmentos
;Ejemplo
(define (segs+img loseg img)
  (cond
    [(empty? loseg) img]
    [else
     (imagen-en-celda
      BODY
      (posn-x (first loseg)) (posn-y (first loseg))
      (segs+img (rest loseg) img))]))
  
;Contrato:food+img: fruta image -> image
;Propósito: Funcion que dibuja las fruta en el canvas. Donde fruta es una estructura
;Ejemplo: 
(define (food+img fruta img)
  (imagen-en-celda APPLE (posn-x fruta) (posn-y fruta) img))
;Contrato: bono+img: world iamgen->image
;Proposito: 
(define (bono+img w img)
  (imagen-en-celda BONO (posn-x (loc-bonus w)) (posn-y (loc-bonus w)) img))

;Contrato: score+img: number --> image
;Propósito: Hacer que el puntaje aparezca durante el juego y se actualice
;(define (score+img score img)
 ; (imagen-en-celda (fig-score (snake-segs score)) 510 15 img))
(define (name+img)
  (text (text-contents nombre) 20 "cyan"))

;Contrato: fig-score: list world->string
;Proposito: Funcion que pinta el score en el mundo
(define (fig-score x w)
  (cond
    [(comiendo-bonus? w) (text (string-append "Score: " (number->string (+ (calc-score x 0) 2))) 20 "white")]
    [else
     (text (string-append "Score: " (number->string (calc-score x 0))) 20 "white")]))
;Contrato: calc-score: list number->number
;Proposito: Funcion que calcula el puntaje que lleva el jugador durante el juego
(define (calc-score serpiente n)
  (cond
    [(empty? serpiente) n]
    [(<= (length serpiente) 1) n]
    [else (calc-score (rest serpiente) (+ n 1))]))
  
;Contrato: last-scene: world->image
;Proposito: Funcion que dibuja la última escena del juego, osea en el momento en el que el jugador pierde.
(define (last-scene w)
  (place-image
   (above
    (text/font "HAS MUERTO" 30 "red" "Times New Roman" 'default 'normal 'bold #f)
    (fig-score (snake-segs (world-snake w)) w))
   (/ ANCHO 2) (/ LARGO 2)
   (render w)))
;;____________________________FUNCIONES PARA EL MOVIMIENTO______________________________
;Contrato: snake-grow: snake -> snake. Donde snake es una estructura
;Proposito: añade un nuevo segmento a la serpiente en la "cabecera" a una direccion dada
;Ejemplo: 
(define (snake-grow snake)
  (make-snake (cons (new-seg (first (snake-segs snake)) (snake-dir snake))
                    (snake-segs snake))
              (snake-dir snake)))
;Contrato: new-seg: snake-dir -> snake-seg
;Proposito: Funcion que crea un nuevo segmento en la serpiente.
;Ejemplo:
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
;Ejemplo:
(define (nuke-last loseg)
  (cond
    [(empty? (rest loseg)) empty]
    [else
     (cons (first loseg) (nuke-last (rest loseg)))]))
;_____________________________________COLISIONES____________________________________
;Contrato: comiendo?: world -> boolean. Donde w es una estructura
;Proposito: Funcion que determina si la cabecera de la serpiente colisiona con una fruta
;Ejemplo
(define (comiendo? w)
  (posn=? (first (snake-segs (world-snake w))) (world-fruta w)))

;Contrato: comiendo-bonus?: world -> boolean
;Proposito: Funcion que determina si la cabeza de la serpiente colisiona con la fruta bono
(define (comiendo-bonus? w)
  (posn=? (first (snake-segs (world-snake w))) (loc-bonus w)))

;Contrato: posn=?: posn posn -> boolean. Donde a y b son puntos 2d
;Proposito: Funcion que determina si dos puntos estan sobrelapados
;Ejemplo
(define (posn=? a b)
  (and (= (posn-x a ) (posn-x b)) (= (posn-y a) (posn-y b))))

;Contrato: self-colission?: world -> boolean. Donde w es una estructura
;Proposito: Funcion que determina si la serpiente se esta chocando con sigo misma
;Ejemplo:
(define (self-collision? w)
  (seg-collision? (first (snake-segs (world-snake w))) (rest (snake-segs (world-snake w)))))

;Contrato: seg-colission?: segs -> boolean
;Proposito: Funcion que determina si un segmento dado esta en el mismo lugar que algún otro en la lista
;Ejemplo:
(define (seg-collision? seg los)
  (cond
    [(empty? los) false]
    [else
     (or (posn=? seg (first los)) (seg-collision? seg (rest los)))]))

;Contrato: world-collision?: world -> boolean. donde w es una estructura
;Proposito: Funcion que determina si la serpiente esta chocando con uno de los bordes del mundo
;Ejemplo:
(define (world-collision? w)
  (not (in-bounds? (first (snake-segs (world-snake w))))))

;Contrato: in-bounds?: p -> boolean. Donde p es un punto 2d
;Proposito: Funcion que determina si un determinado punto esta en el borde del canvas
;Ejemplo:
(define (in-bounds? p)
  (and (>= (posn-x p) 0) (< (posn-x p) N-COLUMNAS)
       (>= (posn-y p) 0) (< (posn-y p) N-FILAS)))
       
  
;Contrato: end?: world->boolean
;Proposito: Funcion que evalúa si el jugador ha perdido. Es decir, si el snake choca con un muro choca consigo mismo
(define (end? w)
  (cond
    [(eqv? (save-game w) "puntajes.txt") true]
    [else false]))
;(or (world-collision? w) (self-collision? w)))
       
;:::::::::::::::::::::::::::::::::::FUNCIONES LOGICAS:::::::::::::::::::::::::::::::::::::::::::
;Contrato: zerobonus?: world->
;Proposito: Funcion que determina si el bonus expiró
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
;Propósito: Funcion que calcula el nuevo estado del mundo en cada tick del reloj
;Ejemplo: 
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
;Ejemplo:
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

;Contrato: save-game: w -> .txt
;Propósito: Funcion que guarda el puntaje del jugador
;Ejemplo
;(save-game WORLD-0) Debe retornar un .txt en el directorio del juego con el puntaje
(define (crear-txt w)
  (write-file "puntajes.txt" (string-append (text-contents nombre) (number->string (calc-score (snake-segs (world-snake w)) 0)))))

(define texto (read-file "puntajes.txt"))
(define lineas (string-split texto "\n"))

(define (lista->score l) (cons (first l) (string->number (first (rest l)))))
(lista->score (string-split (first lineas) ","))

(define (lista->lista-score l)
  (cond
    [(empty? l) empty]
    [else
     (cons (lista->score (string-split (first l) ",")) (lista->lista-score (rest l)))]))

(lista->lista-score lineas)


(define (save-game w)
  (cond
    [(or (world-collision? w) (self-collision? w)) (crear-txt w)]
    [else w]))

;::::::::::::::::::::::::::::::::::::::::::::VENTANAS::::::::::::::::::::::::::::::::

(define header (make-message "
      Culebrita - FDP

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
       (list (make-button "Instrucciones" w3))
       (list (make-button "Salir" (lambda (e) (hide-window w)))))))
  
;;BIG-BANG
;Contrato: main: world->world
;Proposito: Funcion que inicia el juego
(define (main w)
  (big-bang WORLD0
    [to-draw render]
    [on-tick next-world TICK]
    [on-key tecla]
    [stop-when end? last-scene]
    [name "culebrita"]) #t)
