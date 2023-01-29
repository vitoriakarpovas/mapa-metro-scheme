; Gustavo Hiroshi Yoshio - 32033273
; Vitoria Karpovas Chisman - 32008554

(define amarela (list "pinheiros" "fariaLima" "fradiqueCoutinho" "oscarFreire" "paulista" "mackenzie" "republica"))
(define vermelha (list "republica" "santaCecilia" "marechalDeodoro" "palmeiras"))
(define diamante (list "palmeiras" "lapa" "domingos" "leopoldina" "altino"))
(define esmeralda (list "altino" "ceasa" "vilaLobos" 
"universitária" "pinheiros"))

; Dada uma linha, indicar quantas estações tem. 
(define (tam linha)
    (if (null? linha) 0 (+ 1 (tam (cdr linha)))))

(print "Tamanho da linha amarela: " (tam amarela))
(print "Tamanho da linha vermelha: " (tam vermelha))
(print "Tamanho da linha diamante: " (tam diamante))
(print "Tamanho da linha esmeralda: " (tam esmeralda))

(print "\n")
; Dadas duas linhas, indicar onde se encontram. 
(define (encontro linha1 linha2 ponto)
    (if (or (null? linha1) (null? linha2)) #f (if (member (car linha1) linha2) (append ponto (car linha1)) (encontro (cdr linha1) linha2 ponto))))

(define (chamaEncontro x y)
  (if (not (encontro x y '())) (print "Nao existe encontro entre as linhas escolhidas") (print "O encontro entre as linhas escolhidas é no ponto " (encontro x y '()))))

(chamaEncontro amarela vermelha)
(chamaEncontro amarela diamante)
(chamaEncontro amarela esmeralda)
(chamaEncontro vermelha diamante)
(chamaEncontro diamante esmeralda)

(print "\n")
; Dadas duas estações, indicar por qual linha fazer o trajeto entre elas diretamente, sem baldeações
(define (direto x y onde)
  (cond ((and (member x amarela) (member y amarela)) (append onde "amarela"))
        ((and (member x vermelha) (member y vermelha)) (append onde "vermelha"))
        ((and (member x diamante) (member y diamante)) (append onde "diamante"))
        ((and (member x esmeralda) (member y esmeralda)) (append onde "esmeralda"))
        (append onde '())))

(define (chamaDireto x y)
  (if (null? (direto x y '())) (print "Nao existe caminho entre " x " e " y " sem baldiação") (print "O caminho direto entre " x " e " y " é pela linha " (direto x y '()))))

(chamaDireto "domingos" "lapa")
(chamaDireto "republica" "santaCecilia")
(chamaDireto "oscarFreire" "mackenzie")
(chamaDireto "vilaLobos" "ceasa")
(chamaDireto "reboucas" "paulista")

(print "\n")
; Dada uma estação, dizer a qual linha(s) pertence 
(define (pertence y linha)
    (if (null? linha) #f (if (equal? y (car linha)) #t (pertence y (cdr linha)))))

(define (baldiacoa y onde)
  (if (and (pertence y amarela) (pertence y vermelha)) (append onde "amarela, vermelha") (if (and (pertence y amarela) (pertence y diamante)) (append onde "amarela, diamante") (if (and (pertence y amarela) (pertence y esmeralda)) (append onde "amarela, esmeralda") (if (and (pertence y vermelha) (pertence y diamante)) (append onde "vermelha, diamante") (if (and (pertence y vermelha) (pertence y esmeralda)) (append onde "vermelha, esmeralda") (if (and (pertence y diamante) (pertence y esmeralda)) (append onde "diamante, esmeralda") #f)))))))

(define (pertence2 x linhas)
     (cond ((member x amarela) (append linhas "amarela"))
        ((member x vermelha) (append linhas "vermelha"))
        ((member x diamante) (append linhas "diamante"))
        ((member x esmeralda) (append linhas "esmeralda"))
        (print "Nao pertence a nenhuma linha")))

(define (chamaBaldiacao ponto)
  (if (baldiacoa ponto '()) (print "O ponto " ponto " está nas linhas " (baldiacoa ponto '())) (print "O ponto " ponto " está na linha " (pertence2 ponto '()))))

(chamaBaldiacao "lapa")
(chamaBaldiacao "republica")
(chamaBaldiacao "ceasa")
(chamaBaldiacao "altino")

(print "\n")
; Dadas duas estações, identificar o menor caminho (em número de estações) entre elas
(define todosPontos (list "pinheiros" "fariaLima" "fradiqueCoutinho" "oscarFreire" "paulista" "mackenzie" "republica" "santaCecilia" "marechalDeodoro" "palmeiras" "lapa" "domingos" "leopoldina" "altino" "ceasa" "vilaLobos"))

(define (distancia ponto1 ponto2 linha)
   (if (null? linha) (distancia ponto1 ponto2 todosPontos) (if (equal? ponto1 ponto2) 0 (+ 1 (distancia (getNextElement linha ponto1) ponto2 linha)))))

(define (getNextElement linha ponto)
  (if (null? linha) (getNextElement todosPontos ponto) (if (equal? ponto (car linha)) (if (null? (cdr linha)) (car todosPontos) (cadr linha)) (getNextElement (cdr linha) ponto))))

(define (caminho pontoA pontoB linha cam)
   (if (null? linha) (caminho pontoA pontoB todosPontos cam) (if (equal? pontoA pontoB) (begin (set! cam (append cam (list pontoA))) (append cam '())) (begin (set! cam (append cam (list pontoA))) (append cam '()) (caminho (getNextElement linha pontoA) pontoB linha cam)))))

(define (menorCaminho comeco fim)
  (if (< (distancia comeco fim todosPontos) 9) (caminho comeco fim todosPontos '()) (reverse (caminho fim comeco todosPontos '()))))

(define (chamaMenorCaminho x y)
  (print "O menor caminho entre " x " e " y " é por " (menorCaminho x y)))

(chamaMenorCaminho "domingos" "santaCecilia")
(chamaMenorCaminho "republica" "ceasa")
(chamaMenorCaminho "altino" "leopoldina")

(print "\n")
; Dada uma estação, identificar o menor caminho para chegar ao Mackenzie 
(chamaMenorCaminho "altino" "mackenzie") 
(chamaMenorCaminho "paulista" "mackenzie") 
(chamaMenorCaminho "republica" "mackenzie") 
(chamaMenorCaminho "domingos" "mackenzie") 