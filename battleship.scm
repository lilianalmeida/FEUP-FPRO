(define jogar
  (lambda ()
    (jogo-batalha (direcao (tabua1)) 15)))


(define jogo-batalha
  (lambda (tabul max-tentativas)
    (let ((tabuleiro tabul))
      (display "***********************")(newline)
      (display "**** Batalha Naval ****")(newline)
      (display "***********************")(newline)(newline)
      (display "Tente afundar todos os navios em 15 tentativas para ganhar o jogo.")
      (newline)
      (newline)
      (display "Navios:")(newline)
      (display "5 posicoes - 1 navio")(newline)
      (display "4 posicoes - 1 navio")(newline)  
      (display "3 posicoes - 2 navios")(newline)
      (display "2 posicoes - 2 navios")(newline)
      (newline)
      (newline)
      (display-matriz (matriz))
      (newline)
      (display "Indica as coordenadas de uma posicao (na forma letra maiuscula,numero) onde acha que poderá encontrar um navio: ")
      (newline)
      (faz-jogada tabuleiro (matriz) max-tentativas 1)
      )))

;;;;Jogada
(define faz-jogada
  (lambda (tab mat tenta k)
    (letrec ((tentativa (read-line))
             ;erros ao inserir valor
             (erro1
              (lambda()
                (if (= (string-length tentativa) 3)
                    (erro2)
                    (begin   
                      (display "Erro! Tenta novamente!")
                      (newline)(newline)(newline)
                      (display "Indica as coordenadas de uma posicao (na forma letra maiuscula,numero) onde acha que poderá encontrar um navio: ")
                      (newline)
                      (faz-jogada tab mat tenta k))
                    )))
             
             (erro2
              (lambda ()
                (let ((letra (string-ref tentativa 0))
                      (num (string-ref tentativa 2)))
                  (if (or (< (char->integer letra) 65) (> (char->integer letra) 71) (< (char->integer num) 49) (> (char->integer num) 55))
                      (begin 
                        (display "Erro! Tenta novamente!")
                        (newline)(newline)(newline)
                        (display "Indica as coordenadas de uma posicao (na forma letra maiuscula,numero) onde acha que poderá encontrar um navio: ")
                        (newline)
                        (faz-jogada tab mat tenta k))
                      (aux tab)))))
             
             ;;; jogada ;;;
             (aux
              (lambda (t)
                (let ((letra (string-ref tentativa 0))
                      (num (string-ref tentativa 2)))
                  ; repetiu valor?
                  (if (not (equal? "-" (vector-ref (vector-ref mat (- (char->integer letra) 65)) (- (char->integer num) 49))))
                      (begin 
                        (display "Ja inseriste esse valor! Coloca outro!")
                        (newline)(newline)(newline)
                        (display "Indica as coordenadas de uma posicao (na forma letra maiuscula,numero) onde acha que poderá encontrar um navio: ")
                        (newline)
                        (faz-jogada tab mat tenta k))
                      
                      (if (null? t)
                          (begin 
                            (newline)
                            (newline)
                            (display "JORNADA ")
                            (display k)
                            (newline)
                            (matriz-set! mat (char->integer letra) (char->integer num) "*")
                            (display "Acertaste na água, tenta outra vez!")
                            (newline)(newline)
                            (display "Tentativas-restantes: ") (display (sub1 tenta))
                            (newline) 
                            (display "Numero de navios restantes: ") (display (afunda tab 6))
                            (newline) (newline)
                            (display-matriz mat) (newline)
                            (tentas))
                          
                          (if (member (string letra num) (car t))
                              (begin
                                (newline)(newline)
                                (display "JORNADA ")
                                (display k)
                                (newline)
                                (matriz-set! mat (char->integer letra) (char->integer num) "X")
                                (display "Acertaste num navio!")
                                (newline)(newline)
                                (display "Tentativas-restantes: ") (display tenta)
                                (newline) 
                                (display "Numero de navios restantes: ") (display (afunda tab 6))     
                                (newline)(newline)
                                
                                (display-matriz mat)
                                (if (= (afunda tab 6) 0)
                                    (fim)
                                    (begin (newline)
                                           (display "Indica as coordenadas de uma posicao (na forma letra maiuscula,numero) onde acha que poderá encontrar um navio: ")
                                           (newline)
                                           (faz-jogada tab mat tenta (add1 k)))))
                              
                              
                              (aux (cdr t))))))))
             
             ;;caso acerte num navio ver se este afunda
             (afunda
              (lambda(ta n)
                (if (null? ta)
                    n
                    (if (equal? (afun (car ta)) (length (car ta)))
                        (begin (abaixo (car ta))
                               (afunda (cdr ta) (sub1 n)) )
                        
                        (afunda (cdr ta)n)))))
             
             (afun
              (lambda (tp)
                (if (null? tp)
                    0
                    (if (or(equal? "X" (vector-ref (vector-ref mat (- (char->integer (string-ref (car tp) 0)) 65)) (- (char->integer (string-ref (car tp) 1)) 49)))
                           (equal? "O" (vector-ref (vector-ref mat (- (char->integer (string-ref (car tp) 0)) 65)) (- (char->integer (string-ref (car tp) 1)) 49))))
                        (add1 (afun (cdr tp)))
                        (afun (cdr tp))))))
             
             (abaixo
              (lambda (ti)
                (if (not (null? ti))
                    (begin 
                      (matriz-set! mat (char->integer (string-ref (car ti) 0)) (char->integer (string-ref (car ti) 1)) "O")
                      (abaixo (cdr ti))))))
             
             
             
             
             
             ;Se acerta na agua, ve se acabaram as tentativas ou nao
             (tentas
              (lambda ()
                (if (= (sub1 tenta) 0)
                    (begin                            
                      (display "Acabou o numero de tentativas! Tenta outra vez!") (newline)
                      (if (= (- 6 (afunda tab 6)) 1)
                          (display "Foi afundado 1 navio!")
                          (begin (display "Foram afundados ") (display (- 6 (afunda tab 6)))
                                 (display " navios!")))
                      
                      (newline) (newline) )                            
                    
                    (begin 
                      (display "Indica as coordenadas de uma posicao (na forma letra maiuscula,numero) onde acha que poderá encontrar um navio: ")
                      (newline)
                      (faz-jogada tab mat (sub1 tenta) (add1 k)))
                    )))
             
             (fim
              (lambda()
                (display "Conseguiste! Acertaste em todos os navios!")(newline)
                (newline)
                (display "Numero de tentivas usadas: ")(display (- 15 tenta))
                (newline)
                (display "Numero de jogadas realizadas: ") (display k)
                (newline)
                (newline)
                ))
             )
      (erro1)
      )))



;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;MATRIZES;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;;Fazer matriz
(define matriz
  (lambda()
    (letrec ((matri (make-vector 7))
             (aux
              (lambda (i)
                (if (not (equal? i 7))
                    (begin
                      (vector-set! matri i (make-vector 7 "-"))
                      (aux (add1 i)))))))
      (aux 0)
      matri)))


(define matriz1 (matriz))

;altera valores da matriz
(define matriz-set!
  (lambda (matriz l c val)
    
    (vector-set! (vector-ref matriz (- l 65)) (- c 49) val)))


;mostra tabuleiro e legenda
(define display-matriz
  (lambda (matriz)
    ;vetor com o número das colunas
    (letrec((coluna
             (lambda (veto j val)
               (if (not(= j 7))
                   (begin
                     (display "   ")
                     (vector-set! veto j val)
                     (display (vector-ref veto j))
                     
                     (coluna veto (add1 j) (add1 val)))))))
      (begin (coluna (make-vector 7) 0 1)
             (newline)
             ))
    
    (letrec((vet
             (lambda (veto j)
               (if (not(= j 7))
                   (begin
                     (display (vector-ref veto j))
                     (display "   ")
                     (vet veto (add1 j))))))
            
            (aux5
             (lambda (i n)
               (if (not(= i 7))
                   (begin
                     (display (integer->char n))
                     (display "  ")
                     (vet (vector-ref matriz i) 0)
                     (newline)
                     (aux5 (add1 i) (add1 n)))))))
      (aux5 0 65))
    (newline) 
    (display "Legenda: ")(newline)
    (display " X = navio atingido")(newline)
    (display " * = tiro perdido")(newline)
    (display " O = navio afundado") (newline)
    (newline)
    (newline)
    (newline)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;TABULEIRO - random;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tabua1 
  (lambda ()
    (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 '()))))))))



(define direcao
  (lambda (tabua)
    (dirrec tabua 5) (dirrec tabua 4) (dirrec tabua 3) (dirrec tabua 3) (dirrec tabua 2) (dirrec tabua 2)))

(define dirrec
  (lambda(plat nav)
    (let ((h-ou-v (random 2)))
      (if (= h-ou-v 0)
          (navios-h nav plat)
          (navios-v nav plat)))
    plat))




;;;;;;;;;;;;;;;;;;;HORIZONTAL;;;;;;;;;;;;;;;;;;;;;;
(define navios-h
  (lambda (numero plataf) 
    (letrec((linha (random 7))
            (coluna (random 7))
            (letra (integer->char (+ 65 linha)))
            
            (verifica-pos-h
             (lambda (lh ch nh tabh)
               (if (> (+ ch (sub1 nh)) 6)
                   #f
                   (if (equal? nh 0)
                       #t
                       (if (= (procura-na-lista tabh (string lh (integer->char (+ ch 49)))) 1)
                           (verifica-pos-h lh (add1 ch) (sub1 nh) tabh)
                           #f)))))
            
            
            
            (horizontal
             (lambda (tabulei)
               (if (not (verifica-pos-h letra coluna numero tabulei))
                   (dirrec tabulei numero)
                   (if (equal? 0 (car tabulei))
                       (set-car! tabulei (minilista-h numero tabulei coluna '() letra))
                       (horizontal (cdr tabulei)))
                   ))))
      (horizontal plataf))))


(define minilista-h
  (lambda (n pl c nl leta)
    (if (= n 0)
        nl
        (minilista-h (sub1 n) pl (add1 c) (append nl (list (string leta (integer->char (+ 49 c)))))leta)
        
        )))

;;;;;;;;;;;;;;;;;;;;;VERTICAL;;;;;;;;;;;;;;;;;;;;;;;;;
(define navios-v
  (lambda (number plata) 
    (letrec((linh (random 7))
            (colum (random 7))
            
            
            (verifica-pos-v
             (lambda (lv cv nv tabv)
               (if (> (+ lv (sub1 nv)) 6)
                   #f
                   (if (equal? nv 0)
                       #t
                       (if (= (procura-na-lista tabv (string  (integer->char (+ lv 65)) (integer->char (+ cv 49)))) 1)
                           (verifica-pos-v (add1 lv) cv (sub1 nv) tabv) 
                           #f)))))
            
            ;         
            (vertical
             (lambda (tabul)
               (if (not (verifica-pos-v linh colum number tabul))
                   (dirrec plata number)
                   (if (equal? 0 (car tabul))
                       (set-car! tabul (minilista-v number tabul colum linh '()))
                       (vertical (cdr tabul)))))))
      
      
      
      (vertical plata))))


(define minilista-v
  (lambda (n-pos pal colu li newl)
    (if (= n-pos 0)
        newl
        (minilista-v (sub1 n-pos) pal colu (add1 li) (append newl (list (string (integer->char (+ 65 li)) (integer->char (+ 49 colu))))))
        
        )))



;;;Procura na lista;;;;

(define procura-na-lista
  (lambda (lista palav)
    (if (null? lista)
        1
        (if (= (procura-na-mini (car lista) palav) 0)
            0
            (procura-na-lista (cdr lista) palav)))))

(define procura-na-mini
  (lambda (mini palavra)
    (if (or (null? mini) (equal? mini 0))
        1
        (if (equal? palavra (car mini))
            0
            (procura-na-mini (cdr mini) palavra)))))


