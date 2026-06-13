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