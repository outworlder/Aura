(define *database-url*
  (make-parameter "tyr104-sqlite3-v1.db"))

(define-class <table> ()
  (name columns (relationships initform: '())))

(define-class <model> ()
  (table))

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
     (let ([result expression])
       (if (null? result)
	   result
	   (if (list? result)
	       (car result)
	       result))))))

;; Simplification: the first column will be the ID
(define-syntax define-model
  (syntax-rules ()
    ([_ model-name database-table-name (table-columns ...)]
     (let ([table-definition (make <table> 'name 'database-table-name 'columns '(table-columns ...))])
       (define-class model-name (<model>)
	 (table-columns ... (table initform: table-definition) )) ))))

(define-syntax has-many
  (syntax-rules (foreign-key:)
    ([_ parent child method-name foreign-key: column-name]
     (define-method (method-name (model parent))
       (find-all-by-id child (slot-value model (quote column-name)))))))

;(has-many <planet-schematics> <planet-schematics-typemap> typemaps foreign-key: schematicID)

(define-syntax belongs-to
  (syntax-rules (foreign-key:)
    ([_ child parent method-name foreign-key: column-name]
     (define-method (method-name (model child))
       (find-by-id parent (slot-value model (quote column-name) ))))))


;(belongs-to <planet-schematics-typemap> <planet-schematics> schematic foreign-key: schematicID)
(define-method (get-table-class (model <model>))
  (slot-value model 'table))

(define-method (get-table-name (model <model>))
  (slot-value (get-table-class model) 'name))

(define-method (list-columns (model <model>))
  (slot-value (get-table-class model) 'columns))

(define (find-all class #!key conditions)
  (with-model class
              (lambda (class table-name table-columns)
                (load-data class (execute-sql (eval (make-select table-columns table-name conditions: conditions)))))))

(define (find-first class #!key conditions)
  (with-model class
              (lambda (class table-name table-columns)
                (car-if-not-empty (load-data class (execute-sql (eval (append (make-select table-columns table-name conditions: conditions) '((limit 1))))))))))

(define (find-by-id class id)
  (with-model class
              (lambda (class table-name table-columns)
                (car-if-not-empty (load-data class (execute-sql (eval (append (make-select table-columns table-name) `((where (= (quote ,(car table-columns)) ,id)))))))))))

(define (find-all-by-id class id)
  (with-model class
              (lambda (class table-name table-columns)
                (load-data class (execute-sql (eval (append (make-select table-columns table-name) `((where (= (quote ,(car table-columns)) ,id))))))))))


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
