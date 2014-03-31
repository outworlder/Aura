(use coops)
(use ssql)
;; (define-model (<name> <parent>)
;;   table: "name"                         ; Optional
;;   columns: ()
;;   has-many: (<another-model>)
;;   belongs-to: (<model> foreign-key: 'asdf)
;;   has-and-belongs-to-many: ())

;; (callback <model> 'before 'save BODY)

;; (define-model '(<inventory-categories> <model-base>)
;;   table: "invcategories"
;;   columns: '(categoryID categoryName description iconID published)
;;   has-many: '(<inventory-groups> foreign-key: categoryID))

(define-class <model-base> ()
  ((columns-initialized? #f)
   (table "")
   (columns '())
   ))

(define-generic (find <model-base>))
(define-generic (find-all <model-base>))
(define-generic (save! <model-base>))
(define-generic (destroy! <model-base>))

(define-method (find model)
  (ssql->sql #f `(select * (from ,(slot-value model 'table)))))

(define-method (check-availability around: (find <model-base>))
  (print "Around find"))

;; ;;; Without any macros
;; (define-class <inventory-categories> <model-base>
;;   ((table "invcategories")
;;    (columns (col1 col2 col3 col4))
;;    (has-many (<inventory-groups>))))

;; ;;; Functions
;; (find <inventory-categories> conditions: '(where (= id 1)))

;; (find-all <inventory-categories>)
