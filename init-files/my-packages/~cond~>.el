;;; Package --- Summary: ~cond~>.el
;;; Commentary:
;; This package implements clojure' cond~> threading macro


;;; 2
;; da forma como está implementado em clj
;; cada par (teste exp) é tranformado em uma closure com uma variavel generica g
;; dps g é associado ao retorno da primeira expressão e assim sucessivamente

;; quando expandido e entao rodado, o valor está correto
;; se rodar sem expandir apenas o ultimo teste é executado
;; steps possui ~>
(defmacro ~cond~> (x &rest clauses)
  "Conditionally thread X through CLAUSES.
Threads x (via `~>') through each form for which the
corresponding test expression is true.  Note that, unlike cond
branching, `~cond~>' threading does not short circuit after the
first true test expression."
  (declare (debug (form body))
           (indent 1))
  (when (-> clauses length (% 2) (= 1))
    (error "Wrong number of arguments"))
  (let (steps)
    (while clauses
      (let ((test (pop clauses))
            (form (pop clauses)))
        (push `(it (if ,test (-> it ,form) it)) steps)))
    `(let* ((it ,x)
            ,@(nreverse steps))
       it)))

(defmacro ~cond~>> (x &rest clauses)
  "Conditionally thread X through CLAUSES.
Threads x (via `->>') through each form for which the
corresponding test expression is true.  Note that, unlike cond
branching, `~cond~>>' threading does not short circuit after the
first true test expression."
  (declare (debug (form body))
           (indent 1))
  (when (-> clauses length (% 2) (= 1))
    (error "Wrong number of arguments."))
  (let (steps)
    (while clauses
      (let ((test (pop clauses))
            (form (pop clauses)))
        (push `(it (if ,test (->> it ,form) it)) steps)))
    `(let* ((it ,x)
            ,@(nreverse steps))
       it)))

(provide '~cond~>)
;;; ~cond~>.el ends here
