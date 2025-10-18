;;;
;;; Copyright 2009-2012 Yuichiro Moriguchi
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(library (ssql)
  (export *lisql*
          eval-select eval-insert eval-update eval-delete
          eval-sql prepare-ssql
          select-ssql update-ssql!)
  (define *lisql* "Lisp SQL")

  (define (eval-error exp)
    (error "SSQL error"))

  (define (geta l) (car l))
  (define (getd l) (cdr l))
  (define (geta2 l) (if (pair? l) (car l) #f))
  (define (getd2 l) (if (pair? l) (cdr l) #f))
  (define (getad   l) (geta   (getd l)))
  (define (getadd  l) (getad  (getd l)))
  (define (getaddd l) (getadd (getd l)))
  (define (getdd   l) (getd   (getd l)))
  (define (getddd  l) (getdd  (getd l)))
  (define (getdddd l) (getddd (getd l)))

  (define (nthd exp n)
    (if (= n 1) exp (nthd (getd exp) (- n 1))))
  (define (ntha exp n)
    (if (= n 1) (geta exp) (ntha (getd exp) (- n 1))))

  (define (listf . exp)
    (cond ((null? exp) '())
          ((car exp) (cons (car exp) (apply listf (cdr exp))))
          (else (apply listf (cdr exp)))))

  (define (eval-select exp env)
    (trim-both (tree->string (eval-select0 exp env))))
  (define (eval-select0 exp env)
    (let ((g (geta exp)))
      (cond ((eq? g 'select) (eval-select1 (getd exp) env))
            ((eq? g 'union) (eval-union (getd exp) env))
            ((eq? g 'intersect) (eval-intersect (getd exp) env))
            ((eq? g 'except) (eval-except (getd exp) env))
            ((eq? g 'union-all) (eval-union-all (getd exp) env))
            ((eq? g 'intersect-all)
              (eval-intersect-all (getd exp) env))
            ((eq? g 'except-all) (eval-except-all (getd exp) env))
            (else (eval-error exp)))))

  (define (list->ii exp env proc op)
    (cond ((null? exp) (eval-error exp))
           ((null? (cdr exp)) (list (proc (car exp) env)))
           (else (cons (proc (car exp) env)
                    (cons op (list->ii (cdr exp) env proc op))))))

  (define (eval-symbol1 exp env)
    (if (symbol? exp) (symbol->string exp) (eval-error exp)))

  (define (eval-setop exp env op) (list->ii exp env eval-select op))
  (define (eval-union exp env) (eval-setop exp env " UNION "))
  (define (eval-union-all exp env) (eval-setop exp env " UNION ALL "))
  (define (eval-intersect exp env) (eval-setop exp env " INTERSECT "))
  (define (eval-intersect-all exp env)
    (eval-setop exp env " INTERSECT ALL "))
  (define (eval-except exp env) (eval-setop exp env " EXCEPT "))
  (define (eval-except-all exp env) (eval-setop exp env " EXCEPT ALL "))

  (define (eval-select1 exp env)
    (let ((sel "SELECT "))
      (if (eq? (geta2 exp) 'distinct)
          (begin
            (set! sel "SELECT DISTINCT ")
            (set! exp (getd exp))))
      (list
        sel
        (eval-cols (geta exp) env)
        (eval-from (getad exp) env)
        (eval-select-clause (getdd exp) env '()))))

  (define (eval-select-clause exp env processed)
    (cond ((null? exp) '())
          ((memq (geta2 (geta2 exp)) processed) (eval-error env))
          ((eval-where (geta2 exp) env) =>
             (lambda (x)
               (cons x (eval-select-clause 
                         (getd exp)
                         env
                         (cons 'where processed)))))
          ((eval-group-by (geta2 exp) env) =>
             (lambda (x)
               (cons x (eval-select-clause 
                         (getd exp)
                         env
                         (cons 'group-by processed)))))
          ((eval-having (geta2 exp) env) =>
             (lambda (x)
               (cons x (eval-select-clause 
                         (getd exp)
                         env
                         (cons 'having processed)))))
          ((eval-order-by (geta2 exp) env) =>
             (lambda (x)
               (cons x (eval-select-clause 
                         (getd exp)
                         env
                         (cons 'order-by processed)))))
          (else (eval-error env))))

  (define (eval-from exp env)
    (if (eq? (geta exp) 'from)
        (list " FROM " (eval-tables (getd exp) env))
        #f))
  (define (eval-where exp env)
    (if (eq? (geta exp) 'where)
        (list " WHERE "
              (let ((rs (eval-relop (getad exp) env)))
                (if (null? rs) "1 = 1" rs)))
        #f))
  (define (eval-having exp env)
    (if (eq? (geta exp) 'having)
        (list " HAVING "
              (let ((rs (eval-relop (getad exp) env)))
                (if (null? rs) "1 = 1" rs)))
        #f))
  (define (eval-group-by exp env)
    (if (eq? (geta exp) 'group-by)
        (list " GROUP BY "
              (list->ii (getd exp) env (lambda (exp env) exp) ","))
        #f))
  (define (eval-order-by exp env)
    (if (eq? (geta exp) 'order-by)
        (list " ORDER BY "
              (list->ii (getd exp) env (lambda (exp env) exp) ","))
        #f))

  (define (eval-cols exp env) (list->ii exp env eval-cols-as ","))
  (define (eval-cols-as exp env)
    (cond ((and (pair? exp) (eq? (geta exp) 'as))
            (if (not (null? (getddd exp)))
                (eval-error env))
            (list (eval-scalar (getad exp) env) " AS " (getadd exp)))
          (else (eval-scalar exp env))))

  (define (eval-tables exp env)
    (list->ii exp env eval-table-join ","))
  (define (eval-table-join exp env)
    (let ((r (geta2 exp)))
      (cond ((or (eq? r 'join) (eq? r 'inner-join))
              (eval-inner-join (getd exp) env))
            ((eq? r 'left-outer-join)
              (eval-left-outer-join (getd exp) env))
            ((eq? r 'right-outer-join)
              (eval-right-outer-join (getd exp) env))
            ((eq? r 'full-outer-join)
              (eval-full-outer-join (getd exp) env))
            ((eq? r 'natural-join)
              (eval-natural-join (getd exp) env))
            ((or (not r) (eq? r 'as)) (eval-table1 exp env))
            (else (eval-error exp)))))

  (define (eval-join exp env op)
    (list->ii exp env eval-table1-on op))
  (define (eval-join2 exp env op)
    (list->ii exp env eval-table1 op))
  (define (eval-table1-on exp env)
    (cond ((eq? (geta2 exp) 'on)
            (list (eval-table1 (getad  exp) env)
                  " ON "
                  (eval-relop  (getadd exp) env)))
          (else (eval-table1 exp env))))

  (define (eval-inner-join exp env)
    (eval-join exp env " INNER JOIN "))
  (define (eval-left-outer-join exp env)
    (eval-join exp env " LEFT OUTER JOIN "))
  (define (eval-right-outer-join exp env)
    (eval-join exp env " RIGHT OUTER JOIN "))
  (define (eval-full-outer-join exp env)
    (eval-join exp env " FULL OUTER JOIN "))
  (define (eval-natural-join exp env)
    (eval-join2 exp env " NATURAL JOIN "))

  (define (eval-tables-list exp env)
    (list->ii exp env eval-table1 ","))

  (define (eval-table1 exp env)
    (cond ((symbol? exp) (symbol->string exp))
          ((eq? (geta exp) 'as) (eval-table-as (getd exp) env))
          (else (eval-error exp))))

  (define (eval-table-as exp env)
    (let ((ge (geta exp)))
      (if (atom? ge)
          (list (symbol->string ge)
                " AS "
                (symbol->string (getad exp)))
          (let ((r (eval-select (geta exp) env)))
            (cond (r (list "("
                           r
                           ") AS "
                           (symbol->string (geta (getd exp)))))
                  ((symbol? (geta exp))
                  (list (symbol->string (geta exp))
                        " AS "
                        (symbol->string (getad exp))))
                  (else (eval-error exp)))))))

  (define (eval-relop exp env)
    (let ((r (geta exp)))
      (cond ((eq? r 'and) `("(" ,@(eval-and (getd exp) env) ")"))
            ((eq? r 'or) `("(" ,@(eval-or (getd exp) env) ")"))
            ((eq? r 'not)
               (if (not (null? (getdd exp))) (eval-error exp))
               `("(" ,@(eval-not (getad exp) env) ")"))
            ((eq? r 'null?)
               (if (not (null? (getdd exp))) (eval-error exp))
               `(,(getad exp) " IS NULL "))
            ((eq? r 'not-null?)
               (if (not (null? (getdd exp))) (eval-error exp))
               `(,(getad exp) " IS NOT NULL "))
            ((eq? r 'in)
               `(,(getad exp)
                 " IN ("
                 ,@(list->ii (getdd exp) env eval-scalar0 ",")
                 ")"))
            ((eq? r 'between)
               (if (not (null? (getdddd exp))) (eval-error exp))
               `(,(getad exp)
                 " BETWEEN "
                 ,(eval-scalar0 (getadd exp) env)
                 " AND "
                 ,(eval-scalar0 (getaddd exp) env)))
            (else (eval-relop1
                    (geta exp) (getad exp) (getadd exp) env)))))

  (define (eval-and exp env)
    (list->ii exp env eval-relop " AND "))
  (define (eval-or exp env)
    (list->ii exp env eval-relop " OR "))
  (define (eval-not exp env)
    (list "NOT " (eval-relop exp env)))

  (define (eval-relop1 op e1 e2 env)
    (cond ((and (keyword? e1) (not (assq e1 env))) '())
          ((and (keyword? e2) (not (assq e2 env))) '())
          (else
           `(,(eval-scalar0 e1 env) " "
             ,(symbol->string op) " "
             ,(eval-scalar0 e2 env)))))
  (define (eval-scalar0 exp env)
    (if (keyword? exp) "? " (eval-scalar exp env)))

  (define (eval-scalar exp env)
    (cond ((and (pair? exp) (eq? (geta exp) 'select))
            `("(" ,(eval-select1 (getd exp) env) ")"))
          ((pair? exp) (apply-func (geta exp) (getd exp) env))
          ((null? exp) (eval-error exp))
          (else (eval-atom exp env))))
  (define (eval-atom exp env)
    (cond ((string? exp) (list "'" exp "'"))
          (else exp)))

  (define (apply-func fn exp env)
    (let ((r (assq fn sclenv)))
      (if r ((cdr r) exp env)
            (apply-default-func fn exp env))))
  (define (apply-default-func fn exp env)
    `(,(symbol->string fn)
      "(" ,@(list->ii exp env eval-scalar ",") ")"))

  (define (eval+ exp env)
    (cond ((null? exp) 0)
          ((and (pair? exp) (null? (cdr exp))) (car exp))
          (else `("(" ,@(list->ii exp env eval-scalar " + ") ")"))))
  (define (eval* exp env)
    (cond ((null? exp) 1)
          ((and (pair? exp) (null? (cdr exp))) (car exp))
          (else `("(" ,@(list->ii exp env eval-scalar " * ") ")"))))
  (define (eval- exp env)
    (cond ((null? exp) (eval-error env))
          ((and (pair? exp) (null? (cdr exp))) (list "-" (car exp)))
          (else `("(" ,@(list->ii exp env eval-scalar " - ") ")"))))
  (define (eval/ exp env)
    (cond ((null? exp) (eval-error env))
          ((and (pair? exp) (null? (cdr exp))) (list "1/" (car exp)))
          (else `("(" ,@(list->ii exp env eval-scalar " / ") ")"))))
  (define (eval& exp env)
    (cond ((null? exp) "''")
          ((and (pair? exp) (null? (cdr exp))) (car exp))
          (else `("(" ,@(list->ii exp env eval-scalar "||") ")"))))

  (define sclenv
    `((+  . ,eval+)
      (-  . ,eval-)
      (*  . ,eval*)
      (/  . ,eval/)
      (&  . ,eval&)))

  ; UPDATE
  (define (eval-update exp env)
    (trim-both (tree->string (eval-update0 exp env))))
  (define (eval-update0 exp env)
    (list
      "UPDATE "
      (eval-upd-table1 (getad exp) env)
      " SET " (eval-upd-set (getadd exp) env)
      (eval-update-clause (getddd exp) env '())))

  (define (eval-update-clause exp env processed)
    (cond ((null? exp) '())
          ((memq (geta2 (geta2 exp)) processed) (eval-error env))
          ((eval-where (geta2 exp) env) =>
             (lambda (x)
               (cons x (eval-select-clause 
                         (getd exp)
                         env
                         (cons 'where processed)))))
          (else (eval-error env))))

  (define (eval-upd-table1 exp env)
    (cond ((eq? (geta2 exp) 'as)
            (list (getad exp) " AS " (getadd exp)))
          ((symbol? exp) exp)
          (else (eval-error exp))))

  (define (eval-upd-set exp env)
    (if (eq? (geta exp) 'set)
        (list->ii (getd exp) env eval-upd-set1 ",")
        (eval-error env)))

  (define (eval-upd-set1 exp env)
    (list (geta exp) " = " (eval-upd-set1-rvalue (getad exp) env)))

  (define (eval-upd-set1-rvalue exp env)
    (cond ((keyword? exp) " ? ")
          (else (eval-scalar exp env))))

  ; INSERT
  (define (eval-insert exp env)
    (trim-both (tree->string (eval-insert0 exp env))))
  (define (eval-insert0 exp env)
    (list
      "INSERT "
      (eval-ins-into (getad exp) (getadd exp) env)
      (eval-ins-vals (getaddd exp) env)
      (eval-insert-clause (getdddd exp) env '())))

  (define (eval-insert-clause exp env processed)
    (cond ((null? exp) '())
          ((memq (geta2 (geta2 exp)) processed) (eval-error env))
          (else (eval-error env))))

  (define (eval-ins-into tname exp env)
    (list " INTO " tname "(" (eval-ins-cols exp env) ")"))
  (define (eval-ins-cols exp env)
    (cond ((null? exp) '())
          ((pair? exp) (list->ii exp env eval-ins-cols2 ","))
          (else (eval-error exp))))
  (define (eval-ins-cols2 exp env)
    (if (symbol? exp) exp (eval-error exp)))

  (define (eval-ins-vals exp env)
    (cond ((eq? (geta exp) 'values)
            (list " VALUES (" (eval-values (getd exp) env) ")"))
          (else (eval-select1 exp env))))

  (define (eval-values exp env)
    (list->ii exp env eval-insert-scalar ","))
  (define (eval-insert-scalar exp env)
    (cond ((keyword? exp) " ? ")
          (else (eval-scalar exp env))))

  ; DELETE
  (define (eval-delete exp env)
    (trim-both (tree->string (eval-delete0 exp env))))
  (define (eval-delete0 exp env)
    (list
      "DELETE "
      (eval-del-from (getad exp) env)
      (eval-delete-clause (getdd exp) env '())))

  (define (eval-delete-clause exp env processed)
    (cond ((null? exp) '())
          ((memq (geta2 (geta2 exp)) processed) (eval-error env))
          ((eval-where (geta2 exp) env) =>
             (lambda (x)
               (cons x (eval-select-clause 
                         (getd exp)
                         env
                         (cons 'where processed)))))
          (else (eval-error env))))

  (define (eval-del-from exp env)
    (if (eq? (geta exp) 'from)
        (list " FROM " (getad exp)) #f))

  ; SQL
  (define (eval-sql exp env)
    (let ((r (geta2 exp)))
      (cond ((eq? r 'insert) (eval-insert exp env))
            ((eq? r 'update) (eval-update exp env))
            ((eq? r 'delete) (eval-delete exp env))
            (else (eval-select exp env)))))

  ; Java
  (define-java-subr
    prepare-ssql
    net.morilib.lisp.lib.ssql.PrepareSsql)
  (define-java-subr
    jdbc-execute-query
    net.morilib.lisp.lib.ssql.JdbcExecuteQuery)
  (define-java-subr
    jdbc-execute-update!
    net.morilib.lisp.lib.ssql.JdbcExecuteUpdateS)

  ; Query
  (define (select-ssql conn ssql env)
    (let* ((sql (eval-select ssql env))
           (pre (jdbc-prepare conn sql)))
      (begin
        (prepare-ssql pre ssql env)
        (let ((r (jdbc-execute-query pre)))
          r))))

  ; Update
  (define (update-ssql! conn ssql env)
    (let* ((sql (eval-sql ssql env))
           (pre (jdbc-prepare conn sql)))
      (begin
        (prepare-ssql pre ssql env)
        (let ((r (jdbc-execute-update! pre)))
          r))))
  )

(cond
  ((not (defined? *lisql*))
    (import (ssql))))

;; END

