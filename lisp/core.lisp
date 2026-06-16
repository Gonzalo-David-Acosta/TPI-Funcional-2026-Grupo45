
;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Función condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)

  (cond
    ((and (eq color-actual 'en-rojo)
          (eq cambiar-a 'intermitente-rojo))
     (list color-actual "cambiar-a-intermitente-rojo"))
    ((and (eq color-actual 'intermitente-rojo)
          (eq cambiar-a 'amarillo))
    (list color-actual "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo) (eq cambia-a 'intermitente-amarillo));;amarillo llega a intermitencia.
            (list color-actual "cambiar-a-intermitente-amarillo"))
        ((and (eq color-actual 'intermitente-amarillo) (eq cambia-a 'verde))   ;;amarillo llega a verde.
            (list color-actual "cambiar-a-verde"))

        ((and (eq color-actual 'en-verde) (eq cambia-a 'intermitente-verde))   ;;verde llega a intermitencia.
            (list color-actual "cambiar-a-intermitente-verde"))
        ((and (eq color-actual 'intermitente-verde) (eq cambia-a 'rojo))   ;;verde llega a rojo.
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
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: recursiva simple
;; IMPACTO: No destructiva
;; ========================================================
(defun ciclos-por-tiempo (minutos)

    (if (and (numberp minutos) (> (* minutos 60) 306))
	(+ 1 (ciclos-por-tiempo(/ (- (* minutos 60) 306) 60)))
	     0))

;; ========================================================
;; FUNCIÓN: porcentaje
;; NATURALEZA: Pura (Mantiene los mismos parametros)
;; ESTRATEGIA: Orden Superior (Trabaja en conjunto con otras funciones)
;; IMPACTO:No Destructiva (no altera la informacion original)
;; ========================================================

(defun porcentaje (hora)
    (list ;;aplicamos regla de 3 simples a cada uno de los colores para sacar su porcentaje
        'rojo (float (/ (* (contador-rojo hora) 100) hora))
        'amarillo (float (/ (* (contador-amarillo hora) 100) hora))
        'verde (float (/ (* (contador-verde hora) 100) hora))
    )
)
(defun contador-rojo (hora)
    (cond
        ((<= hora 0) 0)
        ((< hora 93) hora) ;;Los segundos que quedan por debajo de los 93seg se suman automaticamente              
        ((<= hora 225) 93) ;;Los segundos que quedan por debajo de 225 (ciclo rojo + ciclo verde) hacen que automaticamente se sumen 93 al total debido a que eso le corresponde al rojo            
        ((> hora 0) (+ 93 (contador-rojo (- hora 225))))
    )
)

(defun contador-amarillo (hora)
    (cond
        ((<= hora 0) 0)
        ((<= hora 93) 0) ;;Si los segundos estan por debajo a los 93 no se suma nada debido a que le corresponde a otro color              
        ((< hora 102) (- hora 93)) ;;Los segundos que queden por encima de los 93 seran sumando al ciclo del amarillo     
        ((> hora 0) (+ 9 (contador-amarillo (- hora 225))))
    )
)

(defun contador-verde (hora)
    (cond
        ((<= hora 0) 0)
        ((<= hora 102) 0) ;;Los segundos por debajo de los 102 ya no seran contados ya que eso corresponde a otros dos ciclos (rojo + amarillo)
        ((< hora 225) (- hora 102)) ;;Los segundos por encima de los 102 seran sumandos ya que corresponde a el color verde, pero estos deben estar por debajo de 225 (un ciclo completo)    
        ((> hora 0) (+ 123 (contador-verde (- hora 225))))
    )
)

  	
