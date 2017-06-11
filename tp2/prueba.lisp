;TP2 interprete de lisp

(defun buscarExpresionAmb (exp amb)
 (if (null amb) nil
	(if (eq (car amb) exp) 
		(cadr amb)
		(if (null (cddr amb))
			nil
			(buscarExpresionAmb exp (cddr amb))
		)
	)
 )
)

(defun evaluarArgumentos (argumentos amb)
	(if (null (cdr argumentos))
		(list (evaluar (car argumentos) amb))
		(append (list (evaluar (car argumentos) amb))
			(evaluarArgumentos (cdr argumentos) amb)
		)
	)
	)

(defun evaluarExpresionDesdeElAmbiente (exp amb)
	(evaluar (cons (buscarExpresionAmb (car exp) amb) (evaluarArgumentos (cdr exp) amb)) amb)
)

(defun armarLista (arg amb)
	(if (null (cdr arg)) (cons (evaluar (car arg) amb) ())
		(cons (evaluar (car arg) amb) (armarLista (cdr arg) amb))
	)
)

;Importe cuando hago quiero agrupar valores de dos listas y voy agarrando de la 
;cabeza utilizo list ya que trabajo con atomos y el const no funciona
;porque necesitaria que el segundo asea una lista
(defun construirAmbiente (variables valor amb)
	(if (null (cdr variables))
		(list (car variables) (evaluar (car valor) amb))
		(append (list (car variables) (evaluar (car valor) amb)) 
			(construirAmbiente (cdr variables) (cdr valor) amb))
	)
)

(defun construirFuncion (exp)
	(nth 2 (car exp))
)

(defun evaluarLambda (exp amb)
		(evaluar (construirFuncion exp) (append (construirAmbiente (cadar exp) (cdr exp) amb) amb))
)

(defun evaluarMapcar (funcion atributos amb)
	(if (null(cdr atributos))
		(list (evaluar (cons funcion (list(car atributos))) amb))
		(append (list (evaluar (cons funcion (list(car atributos))) amb))
			(evaluarMapcar funcion (cdr atributos) amb))
	)
)

(defun evaluar (exp amb)
(if (atom exp)  
	(if (numberp exp) exp
		(if (null exp) nil 
			(if (eq exp 't) t (buscarExpresionAmb exp amb))			
		)
	)
	(cond
		((listp (car exp))
			(if (eq (caar exp) 'lambda)
				(evaluarLambda exp amb)
			)
		)
		((eq (car exp) 'numberp)
			(numberp (evaluar (nth 1 exp) amb))
		)
		((eq (car exp) 'atom)
			(atom (evaluar (nth 1 exp) amb))
		)
		((eq (car exp) 'mapcar)
			(evaluarMapcar (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb) amb)
		)
		((eq (car exp) 'quote) (cadr exp))
		((eq (car exp) 'b) 1)
		((eq (car exp) 'or) 
			(or (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb))
		)
		((eq (car exp) 'and) 
			(and (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb))
		)
		((eq (car exp) 'list)
			(armarLista (cdr exp) amb)
		)

		((eq (car exp) 'cons)
			(cons (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb))
		)
		((eq (car exp) 'car)
			(car (evaluar (nth 1 exp) amb))
			)
		((eq (car exp) 'cdr)
			(cdr (evaluar (nth 1 exp) amb))
		)
		((eq (car exp) '+)
			(+ (evaluar (nth 1 exp) amb)  (evaluar (nth 2 exp) amb) )
		)
		((eq (car exp) '-)
			(- (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb) )
		)
		((eq (car exp) '*)
			(* (evaluar (nth 1 exp) amb)  (evaluar (nth 2 exp) amb) )
		)

		((eq (car exp) 'eq)
			(eq (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb))
		)
		((eq (car exp) 'if)
			(if (evaluar (nth 1 exp) amb)
				(evaluar (nth 2 exp) amb)
				(evaluar (nth 3 exp) amb)
			)
		)
		(t (if (null (buscarExpresionAmb (car exp) amb))
			exp
			(evaluarExpresionDesdeElAmbiente exp amb)
			)
		)
	)	
)
)

