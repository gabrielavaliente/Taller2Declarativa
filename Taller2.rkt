#lang racket

;; --------------------------------------------
;; EJERCICIO 1 – Contar elementos positivos
;; --------------------------------------------

(define (contar-positivos lista)
(length (filter (lambda (x) (> x 0)) lista)))
(displayln "Ejercicio 1) Contar elementos positivos:")
(displayln (contar-positivos '(3 -2 7 0 -5 9)))  

;; --------------------------------------------
;; EJERCICIO 2 – Generar lista de cuadrados pares
;; --------------------------------------------
(define (cuadrados-pares lista)
(map (lambda (x) (* x x))
(filter (lambda (x) (even? x)) lista)))
(displayln "Ejercicio 2) Cuadrados pares:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8))) 

;; --------------------------------------------
;; EJERCICIO 3 – Calcular el factorial de un número
;; --------------------------------------------
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln "Ejercicio 3) Factorial:")
(displayln (factorial 5)) 

;; --------------------------------------------
;; EJERCICIO 4 – Elevar cada número al cubo
;; --------------------------------------------

(define (cubos lista)
  (map (lambda (x) (* x x x)) lista))

(displayln "Ejercicio 4) Cubos:")
(displayln (cubos '(2 3 4)))

;; --------------------------------------------
;; EJERCICIO 5 – Sumar elementos impares
;; --------------------------------------------


(define (suma-impares lista)
  (foldl + 0
         (filter (lambda (x) (odd? x)) lista)))
(displayln "Ejercicio 5) Suma de impares:")
(displayln (suma-impares '(1 2 3 4 5 6 7)))

;; --------------------------------------------
;; EJERCICIO 6 – ¿Contiene números negativos?
;; --------------------------------------------
;; Entrada: '(5 9 -3 2)
;; Salida esperada: #t

(define (contiene-negativos lista)
  (ormap (lambda (x) (< x 0)) lista))
(displayln "Ejercicio 6) Contiene negativos:")
(displayln (contiene-negativos '(5 9 -3 2))) 


;; --------------------------------------------
;; EJERCICIO 7 – Suma acumulada de una lista
;; --------------------------------------------
(define (suma-acumulada lista)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lista)))
(displayln "Ejercicio 7) Suma acumulada:")
(displayln (suma-acumulada '(1 2 3 4)))


;; --------------------------------------------
;; EJERCICIO 8 – Concatenar cadenas con foldl
;; --------------------------------------------

(define (concatenar-cadenas lista)
  (foldl string-append "" lista))
(displayln "Ejercicio 8) Concatenar cadenas:")
(displayln (concatenar-cadenas '("Hola" " " "Mundo"))) 


;; --------------------------------------------
;; EJERCICIO 9 – Doble de números mayores que 5
;; --------------------------------------------
(define (dobles-mayores-que-5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))
(displayln "Ejercicio 9) Dobles mayores que 5:")
(displayln (dobles-mayores-que-5 '(3 6 8 2 10))) 

;; --------------------------------------------
;; EJERCICIO 10 – Invertir una lista
;; --------------------------------------------
(define (invertir-lista lista)
  (foldl (lambda (x acc) (cons x acc))
         '()
         lista))
(displayln "Ejercicio 10) Invertir lista:")
(displayln (invertir-lista '(1 2 3 4))) 


;; --------------------------------------------
;; EJERCICIO 11 – Función de orden superior
;; --------------------------------------------


(define (aplicar-a-lista f lista)
  (map f lista))
(define (cuadrado x)
  (* x x))
(displayln "Ejercicio 11) Función como parámetro:")
(displayln (aplicar-a-lista cuadrado '(1 2 3 4))) 




;; --------------------------------------------
;; EJERCICIO 12 – Promedio de números > 5
;; --------------------------------------------
(define (promedio-mayores-que-5 lista)
  (let* ([filtrados (filter (lambda (x) (> x 5)) lista)]
         [suma      (foldl + 0 filtrados)]
         [cantidad  (length filtrados)])
    (exact->inexact (/ suma cantidad))))

(displayln "Ejercicio 12) Promedio de números > 5:")
(displayln (promedio-mayores-que-5 '(3 8 10 4 9 2 7))) 
