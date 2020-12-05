;;; Package --- Summary: -cond->.el
;;; Commentary:
;; This package implements clojure' cond-> threading macro

;;; Code:
;;; 1
;; Recursão (horrorosa, por sinal, nunca quero ver ela expandida)
;; funciona, mas diferente de -> form precisa ser uma lista

;; (defmacro -cond-> (x test form &rest more)
;;   "Takes an expression as X and a set of TEST/FORM pairs as MORE.
;; Threads expr (via ->) through each form for which the
;; corresponding test expression is true.  Note that, unlike cond
;; branching, cond-> threading does not short circuit after the
;; first true test expression."
;;   (declare (debug (form &rest [&or symbolp (sexp &rest form)]))
;;            (indent 1))
;;   (cl-assert (-> more length (% 2) (= 0)))
;;   (unless (listp form)
;;     (setq form (list form)))
;;   (cond
;;    ((null more)
;;     `(if ,test
;;         (,(car form) ,x ,@(cdr form))
;;       ,x))
;;    (:else
;;     `(if ,test
;;          (-cond-> (,(car form) ,x ,@(cdr form))
;;            ,(car more) ,(cadr more) ,@(cddr more))
;;        (-cond-> ,x ,@more)))))

;;; 2
;; da forma como está implementado em clj
;; cada par (teste exp) é tranformado em uma closure com uma variavel generica g
;; dps g é associado ao retorno da primeira expressão e assim sucessivamente

;; quando expandido e entao rodado, o valor está correto
;; se rodar sem expandir apenas o ultimo teste é executado
;; steps possui ->
(defmacro -cond-> (x &rest clauses)
  "Takes an expression as X and a set of test/form pairs as CLAUSES.
Threads expr (via ->) through each form for which the
corresponding test expression is true.  Note that, unlike cond
branching, cond-> threading does not short circuit after the
first true test expression."
  (declare (debug (form body))
           (indent 1))
  (cl-assert (-> clauses length (% 2) (= 0)))
  ;; (message "%s" clauses)
  (-let* ((it (intern "it"))
          (steps (-map
                  (-lambda ((test step))
                    `(if ,test
                         (-> it ,step)
                       it))
                  (-partition 2 clauses))))
    `(-let* ((it ,x)
            ,@(-zip-lists (-cycle '(it))
                          (butlast steps)))
       ,@(if (null steps)
            it
           (last steps)))))

(defmacro -cond->> (x &rest clauses)
  "Takes an expression as X and a set of test/form pairs as CLAUSES.
Threads expr (via ->) through each form for which the
corresponding test expression is true.  Note that, unlike cond
branching, cond-> threading does not short circuit after the
first true test expression."
  (declare (debug (form body))
           (indent 1))
  (cl-assert (-> clauses length (% 2) (= 0)))
  ;; (message "%s" clauses)
  (-let* ((it (intern "it"))
          (steps (-map
                  (-lambda ((test step))
                    `(if ,test
                         (->> ,it ,step)
                       ,it))
                  (-partition 2 clauses))))
    `(-let* ((,it ,x)
             ,@(-zip-lists (-cycle '(it))
                           (butlast steps)))
       ,@(if (null steps)
            it
           (last steps)))))

;; testes
;; (ert-deftest -cond->test ()
;;   (should (string= "ac"
;;                    (-cond-> ""
;;                      (= 1 1) (concat "a")
;;                      (= 2 1) (concat "b")
;;                      (= 2 2) (concat "c"))))
;;   (should (= 10
;;              (-cond-> 0
;;                nil (- 10)
;;                t (+ 10)))))



(provide '-cond->)
;;; -cond->.el ends here
