(use aql)
(use sql-de-lite)
(use coops)

;; (define-class model () (id))

(define *database-url*
  (make-parameter "tyr104-sqlite3-v1.db"))

(define-class <table> ()
  (name columns (relationships initform: '())))

(define-class <model> ()
  (table))

;; Simplification: the first column will be the ID
(define-syntax define-model
  (syntax-rules ()
    ([_ model-name database-table-name (table-columns ...)]
     (let ([table-definition (make <table> 'name 'database-table-name 'columns '(table-columns ...))])
       (define-class model-name (<model>)
	 (table-columns ... (table initform: table-definition) )) ))))

(define-method (get-table-class (model <model>))
  (slot-value model 'table))

(define-method (get-table-name (model <model>))
  (slot-value (get-table-class model) 'name))

(define-method (list-columns (model <model>))
  (slot-value (get-table-class model) 'columns))

(define (all class #!key conditions)
  (with-model class
              (lambda (class table-name table-columns)
                (load-data class (execute-sql (eval (make-select table-columns table-name conditions: conditions)))))))

(define (first class #!key conditions)
  (with-model class
              (lambda (class table-name table-columns)
                (car-if-not-empty (load-data class (execute-sql (eval (append (make-select table-columns table-name conditions: conditions) '((limit 1))))))))))

(define (find-by-id class id)
  (with-model class
              (lambda (class table-name table-columns)
                (car-if-not-empty (load-data class (execute-sql (eval (append (make-select table-columns table-name) `((where (= (quote ,(car table-columns)) ,id)))))))))))

(define-syntax with-model
  (syntax-rules ()
    ([_ class function]
     (let* ([klass (make class)]
            [table-name (get-table-name klass)]
            [table-columns (list-columns klass)])
       (function class table-name table-columns)))))

(define-syntax car-if-not-empty
  (syntax-rules ()
    ([_ expression]
     (if (null? expression)
         expression
         (if (list? expression)
             (car expression)
             expression)))))

(define (make-select columns table #!key conditions id)
  `(from ,table (,@columns) ,conditions))

(define (make-where-id columns id)
  `(where (= ,(car columns) ,id)))

(define-method (save (model <model>))
  #f)

(define (execute-sql stmt)
  (call-with-database (*database-url*)
		      (lambda (database)
			(print "Executing: " stmt)
			(query fetch-all (sql database stmt)))))

(define (load-data class data)
  (if (null? data)
      #f
      (map (lambda (row)
             (let ([newobject (make class)])
               (for-each (lambda (column model-column)
                           (set! (slot-value newobject model-column) column)) row (list-columns newobject))
               newobject)) data)))

;; (define-syntax has-many
;;   (syntax-rules (through:)
;;     ([_ model through: field]
;;      (define ))))