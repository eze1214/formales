;TP2 interprete de lisp

(defun buscarExpresionAmb (exp amb)
 (if (null amb) nil
	(if (eq (car amb) exp) 
		(cadr amb)
		(buscarExpresionAmb exp (cddr amb))
	)
 )
)

(defun armarLista (arg amb)
	(if (null (cdr arg)) (cons (evaluar (car arg) amb) ())
		(cons (evaluar (car arg) amb) (armarLista (cdr arg) amb))
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
		((eq (car exp) 'quote) (cadr exp))
		((eq (car exp) 'if) (if (evaluar (cadr exp) amb)
								(evaluar (nth 2 exp) amb) 
								(evaluar (nth 3 exp) amb)
							)
		)
		((eq (car exp) 'or) 
			(or (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb))
		)
		((eq (car exp) 'and) 
			(and (evaluar (nth 1 exp) amb) (evaluar (nth 2 exp) amb))
		)
		((eq (car exp) 'list)
			(armarLista (cdr exp) amb)
			)
		((eq (car exp) 'car)
			(car (evaluar (cdr exp) amb))
			)
		(t (car (evaluar (cdr exp) amb)))
	)	
)
)

