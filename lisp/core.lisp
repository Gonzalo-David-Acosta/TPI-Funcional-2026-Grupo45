
;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Función condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)

  (cond
    ((and (eq color-actual 'en-rojo)
          (eq cambiar-a 'intermitente-rojo)) ;; rojo llega a intermitencia.
     (list color-actual "cambiar-a-intermitente-rojo"))
    ((and (eq color-actual 'intermitente-rojo)
          (eq cambiar-a 'amarillo)) ;; rojo llega a amarillo 
    (list color-actual "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'intermitente-amarillo)) ;;amarillo llega a intermitencia.
            (list color-actual "cambiar-a-intermitente-amarillo"))
        ((and (eq color-actual 'intermitente-amarillo) (eq cambiar-a 'verde))   ;;amarillo llega a verde.
            (list color-actual "cambiar-a-verde"))

        ((and (eq color-actual 'en-verde) (eq cambiar-a 'intermitente-verde))   ;;verde llega a intermitencia.
            (list color-actual "cambiar-a-intermitente-verde"))
        ((and (eq color-actual 'intermitente-verde) (eq cambiar-a 'rojo))   ;;verde llega a rojo.
            (list color-actual "cambiar-a-rojo"))
    (t
     (list color-actual 'accion-por-defecto))))

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Función condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (timestamp)
  (let ((instante (mod timestamp (+ 90 3 120 3 6))))

    (cond
      ((< instante 90)
       'en-rojo)
      ((< instante 93) 'intermitente-rojo)
      ((< instante 213)
       'en-verde)
      ((< instante 216) 'intermitente-verde)
      ((< instante 222) 'en-amarillo)
      (t
       'intermitente-amarillo))))

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (Función de logging que escribe en pantalla)
;; ESTRATEGIA: Secuencial (Operaciones de salida)
;; IMPACTO: No destructiva
;; ========================================================
(defun informe (datos)
	(format t "Tiempo ~A: la luz ha cambiado de ~A a ~A ~%" (first datos) (second datos) (third datos))
)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
(defun duracion-ciclo (ciclo)
	(and (consp ciclo) (numberp (first ciclo)) (numberp (second ciclo)) (numberp (third ciclo))
		(+ (first ciclo) (second ciclo) (third ciclo))
	)
)

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
	(cond
		((NULL duracion) "Error: No se puede dar una recomendacion con una duracion invalida")
		((< duracion 35) "Ciclo demasiado corto")
		((> duracion 150) "Ciclo demasiado largo")
		(t "Ciclo optimo")
	)
)

;; ========================================================
;; FUNCIÓN: calcular-ciclos
;; NATURALEZA: Pura
;; ESTRATEGIA: Recursiva de Cola 
;; IMPACTO: No destructiva
;; ========================================================

(defun calcular-ciclos (segundos acumulador)
  (cond 
    ((< segundos 225) acumulador) 
    (t (calcular-ciclos (- segundos 225) (1+ acumulador)))))


;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Función de Llamada Directa
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
  (cond 
    ((not (numberp minutos)) 0)
    (t (calcular-ciclos (* minutos 60) 0))))

;; ========================================================
;; FUNCIÓN: porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA: Orden Superior (depende de funciones auxiliares)
;; IMPACTO: No destructiva
;; ========================================================

(defun porcentaje (hora)
    (list
        'rojo (float (/ (* (contador-rojo hora) 100) hora))
        'amarillo (float (/ (* (contador-amarillo hora) 100) hora))
        'verde (float (/ (* (contador-verde hora) 100) hora))
    )
)

;; ========================================================
;; CONTADOR ROJO (incluye intermitente 3s)
;; ========================================================

(defun contador-rojo (hora)
    (cond
        ((<= hora 0) 0)
        ((< hora 90) hora)          ;; rojo normal
        ((< hora 225) 90)           ;; ciclo completo rojo
        (t (+ 90 (contador-rojo (- hora 225))))
    )
)

;; ========================================================
;; CONTADOR VERDE (incluye intermitente 3s)
;; ========================================================

(defun contador-verde (hora)
    (cond
        ((<= hora 93) 0)
        ((< hora 213) (- hora 93))  ;; verde normal
        ((< hora 216) (+ 120 (- hora 213))) ;; intermitente verde
        ((< hora 225) 123)
        (t (+ 123 (contador-verde (- hora 225))))
    )
)

;; ========================================================
;; CONTADOR AMARILLO (incluye intermitente 3s)
;; ========================================================

(defun contador-amarillo (hora)
    (cond
        ((<= hora 216) 0)
        ((< hora 222) (- hora 216)) ;; amarillo normal
        ((< hora 225) (+ 6 (- hora 222))) ;; intermitente amarillo
        (t (+ 9 (contador-amarillo (- hora 225))))
    )
)
