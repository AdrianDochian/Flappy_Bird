#lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "constants.rkt")
(require "random.rkt")

; După ce definiți structurile lui (get-initial-state) și a păsării, introduceți în prima
; pe cea din urmă. Colțul din stânga sus a păsării se va află inițial la:
;    y = bird-inițial-y
; și x = bird-x.
; (get-initial-state) va fi o funcție care va returna starea inițială a jocului.

(define-struct birdStruct (y ySpeed))

(define-struct pipeStruct (pipe-x pipe-self-gap))

(define-struct stateStruct (bird pipes score))

; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definiți structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugați primul pipe în starea jocului. Acesta se va află inițial în afară ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Atenție! Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
; Atenție! Recomandăm să păstrați în stare colțul din stânga sus al chenarului lipsa
; dintre cele 2 pipe-uri!

; Vrem o modalitate de a păstra scorul jocului. După ce definiți structura
; acestuia, adăugați scorul inițial, adică 0, în starea inițială a jocului.
; Atenție get-initial-state trebuie sa fie o funcție
; și trebuie apelată în restul codului.

(define (get-initial-state)
  (stateStruct
   (birdStruct bird-initial-y 0)
   (cons (pipeStruct scene-width (+ added-number (random random-threshold))) null)
   0))

; După aceasta, implementați un getter care extrage din structura voastră
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află această.

(define (get-bird state)
  (stateStruct-bird state))

(define (get-bird-y bird)
  (birdStruct-y bird))

; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adaugă
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.

(define (next-state-bird bird gravity)
  (match-let ([(birdStruct y ySpeed) bird])
    (struct-copy birdStruct bird [y (+ y ySpeed)] [ySpeed (+ ySpeed gravity)])))

; După aceasta, implementati un getter care extrage din structura voastră
; viteza pe y a păsării.

(define (get-bird-v-y bird)
  (birdStruct-ySpeed bird))

; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definiți funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimba viteza
; pe y a păsării cu -momentum.

(define (next-state-bird-onspace bird momentum)
  (match-let ([(birdStruct y ySpeed) bird])
    (struct-copy birdStruct bird [ySpeed (* -1 momentum)])))

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.

; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.

(define (change current-state pressed-key)
  (match-let ([(stateStruct bird pipes score) current-state])
    (cond [(key=? pressed-key " ") (struct-copy stateStruct current-state [bird (next-state-bird-onspace bird initial-momentum)])]
          [else current-state])))
  
; După ce ați definit structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementați getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.

(define (get-pipes state)
  (stateStruct-pipes state))

; Implementați get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.

(define(get-pipe-x pipe)
  (match-let ([(pipeStruct pipe-x pipe-self-gap) pipe])
    pipe-x))

(define(get-pipe-self-gap pipe)
  (match-let ([(pipeStruct pipe-x pipe-self-gap) pipe])
    pipe-self-gap))

; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va scădea din x-ul fiecărui pipe
; scroll-speed-ul dat.

(define (move-pipes pipes scroll-speed)
  (map (λ(pipe) (pipeStruct (- (get-pipe-x pipe) scroll-speed) (get-pipe-self-gap pipe)) ) pipes))

; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din DREAPTA sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.

(define (clean-pipes pipes)
  (filter (λ(pipe) (> (+ (get-pipe-x pipe) pipe-width) 0)) pipes))

; Vrem să avem un sursa continuă de pipe-uri.
; Implementati funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.

(define (getFarthestPipe-x pipes)
  (foldr (λ(current acc) (if (> (get-pipe-x current) acc) (get-pipe-x current) acc)) (get-pipe-x (car pipes)) pipes))

(define (add-more-pipes pipes)
  (if (< (length pipes) no-pipes)
      (cons (pipeStruct (+ (+ (getFarthestPipe-x pipes) pipe-width) pipe-gap) (+ added-number (random random-threshold))) pipes)
      pipes))

; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
; move-pipes, urmat de clean-pipes, urmat de add-more pipes.

(define (next-state-pipes pipes scroll-speed)
  (move-pipes (clean-pipes (add-more-pipes pipes)) scroll-speed))

; Creați un getter ce va extrage scorul din starea jocului.

(define (get-score state)
  (stateStruct-score state))

; Vrem să creăm logica coliziunii cu pământul.
; Implementati check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.

(define (check-ground-collision bird)
  (if [>= (+ (get-bird-y bird) bird-height) ground-y]
      #t
      #f
      ))

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.

; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.

; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.

(define (invalid-state? state)
  (cond
    [(check-ground-collision (get-bird state)) #t]
    [(check-pipe-collisions (get-bird state) (get-pipes state)) #t]
    [else #f]))

; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementati funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.

(define (check-pipe-collisions bird pipes)
  (cond
    [(null? pipes) #f]
    [(check-top-pipe bird (car pipes)) #t]
    [(check-bottom-pipe bird (car pipes)) #t]
    [else (check-pipe-collisions bird (cdr pipes))]
       ))

(define (check-top-pipe bird pipe)
  (check-collision-rectangles (make-posn bird-x (get-bird-y bird))
                            (make-posn (+ bird-x bird-width) (+ bird-height (get-bird-y bird)))
                              (make-posn (get-pipe-x pipe) (- (get-pipe-self-gap pipe) pipe-height))
                              (make-posn (+ pipe-width (get-pipe-x pipe)) (get-pipe-self-gap pipe))))

(define (check-bottom-pipe bird pipe)
  (check-collision-rectangles (make-posn bird-x (get-bird-y bird))
                            (make-posn (+ bird-x bird-width) (+ bird-height (get-bird-y bird)))
                              (make-posn (get-pipe-x pipe) (+ pipe-self-gap (get-pipe-self-gap pipe)) )
                              (make-posn (+ pipe-width (get-pipe-x pipe)) (+ pipe-self-gap (+ (get-pipe-self-gap pipe) pipe-height)))))

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

;Next-state
; Next-state va fi apelat de big-bang la fiecare cadru, pentru a crea efectul de
; animație. Acesta va primi ca parametru o structură de tip stare, și va întoarce
; starea corespunzătoare următorului cadru.

; Trebuie să integrăm funcția implementată anterior, și anume next-state-bird,
; în next-state.

; Vrem să implementăm logică legată de mișcarea, ștergerea și adăugarea pipe-urilor
; în next-state. Acesta va apela next-state-pipes pe pipe-urile din starea curentă.

; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.

(define (next-state state)
  (match-let ([(stateStruct bird pipes score) state])
    (struct-copy stateStruct state
                 [bird (next-state-bird bird initial-gravity)]
                 [pipes (next-state-pipes pipes initial-scroll-speed)]
                 [score (+ 0.1 score)])))

; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.

; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define pipe-image (rectangle pipe-width pipe-height "solid" "green"))
;(define initial-scene (empty-scene scene-width scene-height))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

(define (draw-frame state)
  (match-let ([(stateStruct bird pipes score) state])
    (place-image bird-image  (+ bird-x (quotient bird-width 2)) (+ (get-bird-y bird) (quotient bird-height 2))
                 (place-image ground-image  (quotient scene-width 2) (+ ground-y (quotient ground-height 2))
                             (place-image (score-to-image score) text-x text-y 
                              [place-pipes pipes initial-scene] )))))
  
; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.
(define (place-pipes pipes scene)
  (if (null? pipes)
      scene
      (place-image pipe-image [+ (get-pipe-x (car pipes)) (quotient pipe-width 2)] (+ (get-pipe-self-gap (car pipes)) (+ pipe-self-gap (quotient pipe-height 2)))
                   (place-image pipe-image [+ (get-pipe-x (car pipes)) (quotient pipe-width 2)] (- (get-pipe-self-gap (car pipes)) (quotient pipe-height 2))
                                (place-pipes (cdr pipes) scene)))))

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
